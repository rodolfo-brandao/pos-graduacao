// List all users
MATCH (u:User)
RETURN u.id, u.name, u.age, u.email;


// List all films with genre
MATCH (f:Film)-[:IN_GENRE]->(g:Genre)
RETURN f.title AS film, g.name AS genre
ORDER BY film;