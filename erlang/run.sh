a=s/\$HOME/
b="\/home\/"
c=$(whoami)
d=/g
s=$a$b$c$d
sed $s yaws.conf > localyaws.conf
yaws -i --conf ./localyaws.conf
rm localyaws.conf
