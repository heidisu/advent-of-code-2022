# Day 9

All possible positions of the head when the tail is touching the head

```
HHH
HTH
HHH
```

All possible positions when head move
```
 HHH
HHHHH
HHTHH
HHHHH
 HHH
```
Of these are the following positions where tail has to move
```
 HHH
H   H
H T H
H   H
 HHH
```

For task 2 there are more options. Let H be the head, A the knot after head, and B the knot after A:

```
  H
 A
B 
```
H moves one up:
```
  H

 A
B 
```
A moves after, which makes B in distance (x + 2, y + 2) from A
```
  H
  A
 
B  
```
This results in four more possibilities, the missing corners of the previous chart

```
*HHH*
H   H
H T H
H   H
*HHH*
```