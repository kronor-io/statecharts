# SCXMLVisualization
SCXML Visualization

This is a version of the fork in HTML5 (JacobeanRnD/SCHVIZ)

Sample: 
![alt tag](https://raw.githubusercontent.com/redrede/SCXMLVisualization/master/img/state-machine.png)
```
<scxml xmlns="http://www.w3.org/2005/07/scxml" version="1.0" datamodel="ecmascript" initial="oven">
   <datamodel>
      <data id="cook_time" expr="5" />
      <data id="door_closed" expr="true" />
      <data id="timer" expr="0" />
   </datamodel>
   <parallel id="oven">
      <state id="engine">
         <initial>
            <transition target="off" />
         </initial>
         <state id="off">
            <transition event="turn.on" target="on" />
         </state>
         <state id="on">
            <initial>
               <transition target="idle" />
            </initial>
            <transition event="turn.off" target="off" />
            <transition cond="timer &amp;gt;= cook_time" target="off" />
            <state id="idle">
               <transition cond="In('closed')" target="cooking" />
            </state>
            <state id="cooking">
               <transition cond="In('open')" target="idle" />
               <transition event="time">
                  <assign location="timer" expr="timer + 1" />
               </transition>
            </state>
         </state>
      </state>
      <state id="door">
         <initial>
            <transition target="closed" />
         </initial>
         <state id="closed">
            <transition event="door.open" target="open" />
         </state>
         <state id="open">
            <transition event="door.close" target="closed" />
         </state>
      </state>
   </parallel>
</scxml>
```
