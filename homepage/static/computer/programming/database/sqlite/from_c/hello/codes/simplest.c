#include <stdio.h>
#include <stdlib.h>
#include <sqlite3.h>

int
main(int argc, char *argv[])
{
	int ret = 0;
	sqlite3 *conn = NULL;

	ret = sqlite3_open("./simplest.sqlite3", &conn);
	if (ret != SQLITE_OK) {
		printf("SQLITE3: sqlite3_open() failed: %d", ret);
		exit(-1); }

	ret = sqlite3_close(conn);
	if (ret != SQLITE_OK) {
		printf ("SQLITE3: sqlite3_close() failed: %d", ret);
		exit(-1); }

	return 0;
}
