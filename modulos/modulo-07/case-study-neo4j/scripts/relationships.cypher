// ==================================
// Streaming platform - Relationships
// ==================================



// ---------- Film -> Genre ----------
UNWIND [
  {filmId: 1, genreId: 2}, // Interstellar      -> Sci-Fi
  {filmId: 2, genreId: 2}, // Inception         -> Sci-Fi
  {filmId: 3, genreId: 2}, // The Matrix        -> Sci-Fi
  {filmId: 4, genreId: 1}, // Arrival           -> Drama
  {filmId: 5, genreId: 3}, // Dark Knight       -> Action
  {filmId: 6, genreId: 4}  // Grand Budapest    -> Comedy
] AS row
MATCH (f:Film {id: row.filmId})
MATCH (g:Genre {id: row.genreId})
MERGE (f)-[:IN_GENRE]->(g);



// ---------- User -> {Friends with} -> Film ----------
UNWIND [
  {userA: 1, userB: 2}, // Alice  -> {Friends with} -> Bob
  {userA: 1, userB: 3}, // Alice  -> {Friends with} -> Carol
  {userA: 2, userB: 3}  // Bob    -> {Friends with} -> Carol
] AS row
MATCH (u1:User {id: row.userA})
MATCH (u2:User {id: row.userB})
MERGE (u1)-[:FRIENDS_WITH]->(u2);



// ---------- User -> {Watched (score)} -> Film ----------
UNWIND [
  {userId: 1, filmId: 1, score: 5}, // Alice  -> {Watched} -> Interstellar
  {userId: 1, filmId: 2, score: 4}, // Alice  -> {Watched} -> Inception
  {userId: 2, filmId: 3, score: 5}, // Bob    -> {Watched} -> The Matrix
  {userId: 2, filmId: 5, score: 4}, // Bob    -> {Watched} -> The Dark Knight
  {userId: 3, filmId: 4, score: 5}, // Carol  -> {Watched} -> Arrival
  {userId: 3, filmId: 6, score: 3}  // Carol  -> {Watched} -> The Grand Budapest Hotel
] AS row
MATCH (u:User {id: row.userId})
MATCH (f:Film {id: row.filmId})
MERGE (u)-[w:WATCHED]->(f)
SET w.score = row.score;



// ---------- User -> {Wants to watch} -> Film ----------
UNWIND [
  {userId: 1, filmId: 4}, // Alice  -> {Wants to watch} -> Arrival
  {userId: 1, filmId: 6}, // Alice  -> {Wants to watch} -> Grand Budapest

  {userId: 2, filmId: 1}, // Bob    -> {Wants to watch} -> Interstellar
  {userId: 2, filmId: 2}, // Bob    -> {Wants to watch} -> Inception

  {userId: 3, filmId: 3}, // Carol  -> {Wants to watch} -> The Matrix
  {userId: 3, filmId: 5}  // Carol  -> {Wants to watch} -> Dark Knight
] AS row
MATCH (u:User {id: row.userId})
MATCH (f:Film {id: row.filmId})
MERGE (u)-[r:WANTS_TO_WATCH]->(f);