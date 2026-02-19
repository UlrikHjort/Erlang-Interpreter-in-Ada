% Example: Direct pattern matching in assignments
% Demonstrates tuple and list destructuring

% Define a function that returns a complex structure
make_person(Name, Age, City) -> {person, Name, Age, City}.

% Call it and destructure the result
{person, PersonName, PersonAge, PersonCity} = make_person(alice, 30, stockholm)
