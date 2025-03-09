-module(records).
-export(
  [first_robot/0
  ,car_factory/1
  ,admin_panel/1
  ,adult_section/1
  ,repairman/1
  ,included/0
]).

-record(robot, 
  {name
  ,type=industrial
  ,hobbies
  ,details=[]
}).

-record(user, {id, name, group, age}).

-include("records.hrl").

included() -> #included{some_field="Some value"}.

first_robot() ->
  #robot{name="Mechatron"
        ,type=handmade
        ,details=["Moved by a small man inside"]}.

car_factory(CorpName) ->
  #robot{name=CorpName, hobbies="building cars"}.

admin_panel(#user{name=Name, group=admin}) ->
  Name ++ " is allowed!";
admin_panel(#user{name=Name}) ->  
  Name ++ " is not allowed".

adult_section(U = #user{}) when U#user.age >= 18 ->
  allowed;
adult_section(_) ->
  forbidden.

repairman(Rob) ->
  Details = Rob#robot.details,
  NewRob = Rob#robot{details=["Repaired by repairman"|Details]},
  {repaired, NewRob}.
