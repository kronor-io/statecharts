@startuml

hide empty description

note "chart2 version 2.0" as Name

[*] --> s1
state s1 {
  s1 : s1n
  [*] --> s2
  state s2 {
    s2 : s2n
    [*] --> s3
    state s3
    s3 : s3n
    s3 --> s4 : foo.ev10
    s3 : onentry table1.action1
    state s4
    s4 : s4n
    s4 --> s5 : foo.ev9
    s4 : onentry table1.action2
    state s5
    s5 : s5n
    s5 --> s6 : foo.ev8
    s5 : onentry table1.action3
    state s6
    s6 : s6n
    s6 --> s7 : foo.abc7
    s6 : onentry table1.action4
    state s7
    s7 : s7n
    s7 --> [*]
    s7 : onentry table1.action5
  }
  s2 --> s8 : foo.abc6
  state s8 {
    s8 : s8n
    [*] --> s9
    state s9
    s9 : s9n
    s9 --> s10 : foo.abc5
    s9 --> s10 : foo.abc4
    s9 : onentry table1.action7
    state s10
    s10 : s10n
    s10 --> [*]
    s10 : onentry table1.action6
  }
}
s1 --> s11 : bar.abc3
s1 --> s12 : bar.abc2
s1 --> s13 : foo.abc1
state s11
s11 : s11n
s11 --> [*]
state s12
s12 : s12n
s12 --> [*]
state s13
s13 : s13n
s13 --> [*]
s13 : onentry table1.action11

@enduml
