
#include <string.h>

#include <caml/callback.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/threads.h>

#include <mysql.h>

my_bool guira_init(UDF_INIT *initid, UDF_ARGS *args, char *msg)
{
	static value *parse_f = NULL;

	if (args->arg_count != 4)
		return strcpy(msg, "GUIRA() requires four arguments"), 1;

	if (args->arg_type[0] != STRING_RESULT
	    || args->arg_type[1] != STRING_RESULT
	    || args->arg_type[2] != STRING_RESULT
	    || args->arg_type[3] != STRING_RESULT)
		return strcpy(msg, "invalid arguments"), 1;

	if (args->maybe_null[0] || args->maybe_null[1] || args->maybe_null[2])
		return strcpy(msg, "argument cannot be null"), 1;

	if (args->args[2] != 0) {
		value str;
		int did_register = caml_c_thread_register();

		caml_acquire_runtime_system();

		/* this works because 'value' is the size of a pointer */
		initid->ptr = (void *)caml_alloc(1, 0);
		caml_register_generational_global_root((value *)&initid->ptr);

		str = caml_alloc_string(args->lengths[2]);
		memcpy(String_val(str), args->args[2], args->lengths[2]);

		if (parse_f == NULL)
			parse_f = caml_named_value("parse");

		initid->ptr = (void *)caml_callback(*parse_f, str);

		caml_release_runtime_system();
		if (did_register)
			caml_c_thread_unregister();
	} else {
		initid->ptr = NULL;
	}

	return 0;
}

void guira_deinit(UDF_INIT *initid)
{
	int did_register;

	if (initid->ptr == NULL)
		return;

	did_register = caml_c_thread_register();
	caml_acquire_runtime_system();
	caml_remove_generational_global_root((value *)&initid->ptr);
	caml_release_runtime_system();
	if (did_register)
		caml_c_thread_unregister();
}

long long guira(UDF_INIT *initid, UDF_ARGS *args, char *is_null, char *error)
{
	static value *filter_f = NULL;
	static value *parse_f = NULL;
	int did_register, ret;
	value fa[4];

	did_register = caml_c_thread_register();
	caml_acquire_runtime_system();

	if (args->args[3] == 0)
		return *is_null = 1, 0;

	if (initid->ptr == NULL) {
		value str = caml_alloc_string(args->lengths[2]);

		memcpy(String_val(str), args->args[2], args->lengths[2]);

		if (parse_f == NULL)
			parse_f = caml_named_value("parse");
		fa[2] = caml_callback(*parse_f, str);
	} else {
		fa[2] = (value)initid->ptr;
	}

	fa[0] = caml_alloc_string(args->lengths[0]);
	fa[1] = caml_alloc_string(args->lengths[1]);
	fa[3] = caml_alloc_string(args->lengths[3]);
	memcpy(String_val(fa[0]), args->args[0], args->lengths[0]);
	memcpy(String_val(fa[1]), args->args[1], args->lengths[1]);
	memcpy(String_val(fa[3]), args->args[3], args->lengths[3]);

	if (filter_f == NULL)
		filter_f = caml_named_value("filter");

	ret = Bool_val(caml_callbackN(*filter_f, 4, fa));

	caml_release_runtime_system();
	if (did_register)
		caml_c_thread_unregister();

	return ret;
}

static void __attribute__((constructor)) onload()
{
	char *fake_arg[] = { NULL };

	caml_startup(fake_arg);
	caml_release_runtime_system();
}
