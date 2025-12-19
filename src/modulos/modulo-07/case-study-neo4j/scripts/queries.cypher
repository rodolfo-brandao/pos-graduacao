// List all users
MATCH (u:User)
RETURN u.id, u.name, u.age, u.email;



// List Alice's friends
MATCH (u:User {id: 1})-[:FRIENDS_WITH]-(friend:User)
RETURN friend.name;

// List Bob's friends
MATCH (u:User {id: 2})-[:FRIENDS_WITH]-(friend:User)
RETURN friend.name;

// List Carol's friends
MATCH (u:User {id: 3})-[:FRIENDS_WITH]-(friend:User)
RETURN friend.name;

// List all friendships
MATCH (u1:User)-[:FRIENDS_WITH]-(u2:User)
RETURN u1.name AS user1, u2.name AS user2;



// List all films with genre
MATCH (f:Film)-[:IN_GENRE]->(g:Genre)
RETURN f.title AS film, g.name AS genre
ORDER BY film;



// List which films each user watched (with scores)
MATCH (u:User)-[w:WATCHED]->(f:Film)
RETURN u.name AS user, f.title AS film, w.score AS score
ORDER BY user, score DESC;



// List films that friends liked but the user haven't watched
// E.g: User: Alice (id: 1), Score: >= 4
MATCH (me:User {id: 1})-[:FRIENDS_WITH]-(friend:User)
MATCH (friend)-[w:WATCHED]->(f:Film)
WHERE w.score >= 4
  AND NOT (me)-[:WATCHED]->(f)
RETURN
  f.id    AS filmId,
  f.title AS title,
  f.year  AS year,
  avg(w.score)      AS avgFriendScore,
  count(DISTINCT friend) AS friendsWhoLiked
ORDER BY friendsWhoLiked DESC, avgFriendScore DESC, year DESC;



// List each user's watchlist
MATCH (u:User)-[r:WANTS_TO_WATCH]->(f:Film)
RETURN u.name AS user, f.title AS film
ORDER BY user, film ASC;



// Recommend films based on friend's watchlist
// E.g: Carol (id: 3)
MATCH (me:User {id: 3})-[:FRIENDS_WITH]-(friend:User)
MATCH (friend)-[:WANTS_TO_WATCH]->(f:Film)
WHERE NOT (me)-[:WATCHED]->(f)
  AND NOT (me)-[:WANTS_TO_WATCH]->(f)
RETURN f.title AS recommended, count(DISTINCT friend) AS friendsInterested
ORDER BY friendsInterested DESC, recommended;