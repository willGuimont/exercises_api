curl -X POST -H "Content-Type: application/json" -d '{"name": "pushup", "equipment": "BodyWeight"}' http://localhost:8080/exercises
curl -X POST -H "Content-Type: application/json" -d '{"name": "curl", "equipment": "Dumbbell"}' http://localhost:8080/exercises
curl http://localhost:8080/exercises

curl -X POST -H "Content-Type: application/json" -d '{"exerciseId": 1, "volume": "Reps(20)"}' http://localhost:8080/sets
curl -X POST -H "Content-Type: application/json" -d '{"exerciseId": 2, "volume": "Weight(25, 12)"}' http://localhost:8080/sets
curl http://localhost:8080/sets

curl -X POST -H "Content-Type: application/json" -d '{"name": "Arm day", "sets": [[1, 3], [2, 3]]}' http://localhost:8080/plans
curl http://localhost:8080/plans
