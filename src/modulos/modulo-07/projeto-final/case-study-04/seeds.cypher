// ============================
// Streaming platform - Seeds
// ============================


// ---------- Users ----------
MERGE (u1:User {id: 1})
SET u1.name  = "Alice",
    u1.age   = 30,
    u1.email = "alice@streaming.com";

MERGE (u2:User {id: 2})
SET u2.name  = "Bob",
    u2.age   = 25,
    u2.email = "bob@streaming.com";

MERGE (u3:User {id: 3})
SET u3.name  = "Carol",
    u3.age   = 35,
    u3.email = "carol@streaming.com";


// ---------- Genres ----------
MERGE (g1:Genre {id: 1})
SET g1.name = "Drama";

MERGE (g2:Genre {id: 2})
SET g2.name = "Sci-Fi";

MERGE (g3:Genre {id: 3})
SET g3.name = "Action";

MERGE (g4:Genre {id: 4})
SET g4.name = "Comedy";


// ---------- Films ----------
MERGE (f1:Film {id: 1})
SET f1.title = "Interstellar",
    f1.year  = 2014;

MERGE (f2:Film {id: 2})
SET f2.title = "Inception",
    f2.year  = 2010;

MERGE (f3:Film {id: 3})
SET f3.title = "The Matrix",
    f3.year  = 1999;

MERGE (f4:Film {id: 4})
SET f4.title = "Arrival",
    f4.year  = 2016;

MERGE (f5:Film {id: 5})
SET f5.title = "The Dark Knight",
    f5.year  = 2008;

MERGE (f6:Film {id: 6})
SET f6.title = "The Grand Budapest Hotel",
    f6.year  = 2014;