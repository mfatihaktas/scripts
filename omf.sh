#!/bin/bash
echo $1 $2 $3

NODE=node1-1 # all
IMAGE=mininet

if [ $1 = 'stat' ]; then
  omf stat -t all
elif [ $1 = 'tell' ]; then
  omf tell -a on -t all
elif [ $1 = 'loadu' ]; then
  omf load -i baseline.ndz -t $NODE
elif [ $1 = 'load' ]; then
  omf load -i $IMAGE -t $NODE
elif [ $1 = 'save' ]; then
  omf save -n $NODE.sb1.orbit-lab.org
elif [ $1 = 'ssh' ]; then
  ssh -Y root@$NODE
elif [ $1 = 'sshf' ]; then
  ssh frisbee
else
  echo "Argument did not match !"
fi
