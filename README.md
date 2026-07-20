# cellar

Manage a wine cellar from Emacs using SQLite.

## Requirements

- Emacs 30.1+
- SQLite 3.44.0+

## Setup

Create the database from the included schema:

```sh
sqlite3 ~/path/to/inventory.db < schema.sql
```

Then set up `cellar-database` and `cellar-note-dir`.

