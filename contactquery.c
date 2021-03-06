#define _GNU_SOURCE

#include <string.h>
#include <stdlib.h>
#include <glib/gprintf.h>

#if MODERN_PLATFORM
	#include <libebook-contacts/libebook-contacts.h>
#else
	#include <libebook/e-vcard.h>
#endif

#define ARRAY_SIZE(x) (sizeof(x)/sizeof(x[0]))

static const gchar *FILE_EXTENSION = ".vcf";
static const gchar VCARD_DEFAULT_PREF = 100;
static const gchar *EVC_PREF = "PREF";

static const gchar const * MATCH_FIELDS[] = {
	EVC_EMAIL,
	EVC_FN,
};

static void emit_header(void)
{
	g_printf("Searching...\n");
}

static gboolean valid_filename(const gchar *const filename)
{
	return g_str_has_suffix(filename, FILE_EXTENSION);
}

static const gchar *get_attr(EVCard *card, const gchar *name)
{
	EVCardAttribute *attr;

	attr = e_vcard_get_attribute(card, name);
	if (NULL == attr)
		return NULL;
	else
		return  e_vcard_attribute_get_value(attr);
}

static gboolean match_contact(EVCard *card, const gchar *query,
		              const gchar* attr)
{
	const gchar *attr_value;

	attr_value = get_attr(card, attr);

	if (NULL == attr_value)
		return FALSE;
	else
		return !!strcasestr(attr_value, query);
}

static void emit_email(const gchar *email, const gchar *name, const gchar* tags)
{
	if (NULL == tags)
		g_printf("%s\t%s\n", email, name);
	else
		g_printf("%s\t%s\t%s\n", email, name, tags);
}

static const gchar *attr_type(EVCardAttribute *attr) {
	GList *params, *i;

	params = e_vcard_attribute_get_param(attr, EVC_TYPE);

	i = params;
	while (NULL != i)
	{
		GList *next = i->next;
		const gchar *param = i->data;

		if (NULL != param)
			return param;

		i = next;
	}

	return NULL;
}

static gint attr_pref(EVCardAttribute *attr) {
	GList *params, *i;

	params = e_vcard_attribute_get_param(attr, EVC_PREF);

	i = params;
	while (i != NULL)
	{
		GList *next = i->next;
		gchar *param = i->data;
		gchar *endptr = NULL;
		gint64 pref = g_ascii_strtoll(param, &endptr, 10);

		if (NULL != param && 0 == *endptr) /* everything valid */
			return pref;

		i = next;
	}

	return VCARD_DEFAULT_PREF;
}

static gint cmp_attr_pref(gconstpointer a, gconstpointer b) {
	EVCardAttribute *one, *two;
	int pref_one, pref_two;

	one = (EVCardAttribute *)a;
	two = (EVCardAttribute *)b;

	pref_one = attr_pref(one);
	pref_two = attr_pref(two);

	return pref_one - pref_two;
}

static void emit_contact(EVCard *card)
{
	const gchar *name;
	GList *attributes, *i;

	name = get_attr(card, EVC_FN);
	attributes = e_vcard_get_attributes(card);

	i = attributes;
	while (NULL != i)
	{
		GList *next = i->next;
		EVCardAttribute *attribute = i->data;
		const gchar *type = e_vcard_attribute_get_name(attribute);

		if (!!strcmp(EVC_EMAIL, type))
		{
			attribute = NULL;
			/* g_free(i->data);  segfaults, meh */
			attributes = g_list_delete_link(attributes, i);
		}
		i = next;
	}

	attributes = g_list_sort(attributes, cmp_attr_pref);

	i = attributes;
	while (NULL != i)
	{
		GList *next = i->next;
		EVCardAttribute *attribute = i->data;
		const gchar *type = attr_type(attribute);
		const gchar *tag = NULL;
		if (NULL != type && strcasecmp(type, "internet"))
				tag = g_ascii_strdown(type, -1);
		else
			tag = NULL;
		emit_email(e_vcard_attribute_get_value(attribute), name, tag);
		i = next;
	}
}

static gboolean isdir(const gchar *path)
{
	return !!g_file_test(path, G_FILE_TEST_IS_DIR);
}

static gboolean handle_file(const gchar *file, const gchar *query)
{
	if (valid_filename(file) && !isdir(file)) {

		EVCard *card;
		gchar *contents;

		if (!g_file_get_contents(file, &contents, NULL, NULL))
			return FALSE;

		card = e_vcard_new_from_string(contents);

		for (int i = 0; i < ARRAY_SIZE(MATCH_FIELDS); i++) {
			if (match_contact(card, query, MATCH_FIELDS[i])) {
				emit_contact(card);
				break;
			}
		}
	}

	return TRUE;
}

int main(int argc, char **argv)
{
	if (3 != argc) {
		g_fprintf(stderr, "Need exactly 2 arguments\n");
		return 2;
	}

	const gchar *query, *dirname;
	GDir *basedir;

	dirname = argv[1];
	query = argv[2];

#ifndef GLIB_VERSION_2_36
	g_type_init();
#endif

	basedir = g_dir_open(dirname, 0, NULL);

	if (NULL == basedir) {
		g_fprintf(stderr, "Invalid directory\n");
		return 3;
	}

	emit_header();

	const gchar *filename;

	while ((filename = g_dir_read_name(basedir))) {
		gchar *complete_path;

		complete_path = g_build_filename(dirname, filename, NULL);

		if (!handle_file(complete_path, query)) {
			g_fprintf(stderr, "Error reading %s, aborting\n",
					filename);
			return 5;
		}

	}

	return EXIT_SUCCESS;
}
