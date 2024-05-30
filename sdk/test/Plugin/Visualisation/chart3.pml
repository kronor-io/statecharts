@startuml

hide empty description

note "chart3 version 0.3" as Name

[*] --> s1
state s1
s1 : s1n
s1 --> s2 : ev1
s1 : onentry table1.action5
state s2
s2 : s2n
s2 --> s3 : ev2
s2 --> s9 : ev3
s2 --> s7 : ev4
s2 : onentry table1.action4
state s3 {
  s3 : s3n
  [*] --> s4
  state s4
  s4 : s4n
  s4 --> s5 : ev9
  state s5
  s5 : s5n
  s5 --> s4 : ev10
  s5 : onentry table1.action2
  s3 : onentry table1.action3
}
s3 --> s6 : ev5
s3 --> s8 : ev6
s3 --> s9 : ev7
s3 --> s7 : ev8
state s6
s6 : s6n
s6 --> [*]
s6 : onentry table1.action1
state s7
s7 : s7n
s7 --> [*]
state s8
s8 : s8n
s8 --> [*]
state s9
s9 : s9n
s9 --> [*]

@enduml
