// ============================
// Streaming platform - Relationships
// ============================


// ---------- Film ↔ Genre ----------
// Interstellar → Sci-Fi
MERGE (f1)-[:IN_GENRE]->(g2);

// Inception → Sci-Fi
MERGE (f2)-[:IN_GENRE]->(g2);

// The Matrix → Sci-Fi
MERGE (f3)-[:IN_GENRE]->(g2);


// Arrival → Drama
MERGE (f4)-[:IN_GENRE]->(g1);

// The Dark Knight → Action
MERGE (f5)-[:IN_GENRE]->(g3);

// The Grand Budapest Hotel → Comedy
MERGE (f6)-[:IN_GENRE]->(g4);