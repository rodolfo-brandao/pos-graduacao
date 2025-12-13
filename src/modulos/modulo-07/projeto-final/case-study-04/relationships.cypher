// ============================
// Streaming platform - Relationships
// ============================


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