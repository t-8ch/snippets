#define _GNU_SOURCE

#include <string.h>
#include <libebook-contacts/libebook-contacts.h>

static const gchar *FILE_EXTENSION = ".vcf";

static void emit_header(void)
{
	printf("Searching...\n");
}

static gboolean valid_filename(const gchar *filename)
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

static gboolean match_contact(EVCard *card, const gchar *query)
{
	const gchar *name, *email;

	name = get_attr(card, EVC_FN);
	email = get_attr(card, EVC_EMAIL);

	if (NULL == email || strlen(email) == 0)
		return FALSE;
	else

		return !!strcasestr(name, query);
}

static void emit_email_name(const gchar *email, const gchar *name)
{
	printf("%s\t%s\n", email, name);
}

static void emit_emails(gpointer data, gpointer user_data) {

	const gchar *search_name, *attr_type, *email;
	EVCardAttribute *attribute;

	search_name = (gchar *) user_data;
	attribute = (EVCardAttribute *) data;

	attr_type = e_vcard_attribute_get_name(attribute);

	if (!!strcmp(EVC_EMAIL, attr_type))
		return;

	email = e_vcard_attribute_get_value(attribute);

	if (NULL == email)
		return;

	emit_email_name(email, search_name);
}

static void emit_contact(EVCard *card)
{
	const gchar *name;

	name = get_attr(card, EVC_FN);
	g_list_foreach(e_vcard_get_attributes(card), emit_emails, (gpointer) name);
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

		if (match_contact(card, query))
			emit_contact(card);

	}

	return TRUE;
}

int main(int argc, char **argv)
{
	if (argc != 3) {
		fprintf(stderr, "Need exactly 2 arguments\n");
		return 2;
	}

	const gchar *query, *dirname;
	GDir *basedir;

	dirname = argv[1];
	query = argv[2];

	basedir = g_dir_open(dirname, 0, NULL);

	if (NULL == basedir) {
		fprintf(stderr, "Invalid directory\n");
		return 3;
	}

	emit_header();

	const gchar *filename;

	while ((filename = g_dir_read_name(basedir))) {
		gchar *complete_path;

		complete_path = g_build_filename(dirname, filename, NULL);

		if (!handle_file(complete_path, query)) {
			fprintf(stderr, "Error reading %s, aborting\n",
					filename);
			return 5;
		}

	}

	return EXIT_SUCCESS;
}
