// ============================
// Streaming platform - Schema
// ============================


// ---------- Users ----------
CREATE CONSTRAINT user_id_unique IF NOT EXISTS
FOR (u:User)
REQUIRE u.id IS UNIQUE;

CREATE CONSTRAINT user_email_unique IF NOT EXISTS
FOR (u:User)
REQUIRE u.email IS UNIQUE;


// ---------- Films ----------
CREATE CONSTRAINT film_id_unique IF NOT EXISTS
FOR (f:Film)
REQUIRE f.id IS UNIQUE;

CREATE CONSTRAINT film_title_exists IF NOT EXISTS
FOR (f:Film)
REQUIRE f.title IS NOT NULL;

CREATE INDEX film_year_index IF NOT EXISTS
FOR (f:Film)
ON (f.year);


// ---------- Genres ----------
CREATE CONSTRAINT genre_id_unique IF NOT EXISTS
FOR (g:Genre)
REQUIRE g.id IS UNIQUE;

CREATE CONSTRAINT genre_name_unique IF NOT EXISTS
FOR (g:Genre)
REQUIRE g.name IS UNIQUE;