@startuml

hide empty description

note "chart1 version 1.1" as Name

[*] --> s1
state s1 {
  s1 : s1n
  [*] --> s2
  state s2
  s2 : s2n
  s2 --> s3 : ev3
  s2 : onexit table1.action3
  s2 : onexit table1.action4
  state s3
  s3 : s3n
  s3 --> s4 : ev4
  state s4
  s4 : s4n
  s4 --> [*]
  s4 : onentry table1.action1
  s4 : onentry table1.action2
}
s1 --> s16 : ev1
s1 --> s5 : ev2
state s5 {
  s5 : s5n
  [*] --> s6
  state s6
  s6 : s6n
  s6 --> s8 : ev6
  s6 --> s9 : ev7
  s6 --> s17 : ev8
  s6 : onentry table2.action
  state s7
  s7 : s7n
  state s8
  s8 : s8n
  s8 --> s9 : ev8
  s8 --> s7 : ev9
  state s17
  s17 : s17n
  s17 --> [*]
  state s9
  s9 : s9n
  s9 --> [*]
}
s5 --> s10 : ev5
state s10 {
  s10 : s10n
  [*] --> s11
  state s11
  s11 : s11n
  s11 --> s14 : ev10
  s11 --> s12 : ev11
  state s12
  s12 : s12n
  s12 --> s14 : ev12
  s12 --> s15 : ev13
  s12 --> s13 : ev14
  state s13
  s13 : s13n
  s13 --> s14 : ev15
  s13 --> s15 : ev16
  s13 --> s14 : ev17
  s13 --> s15 : ev18
  state s14
  s14 : s14n
  s14 --> [*]
  state s15
  s15 : s15n
  s15 --> [*]
}
state s16
s16 : s16n
s16 --> [*]

@enduml
