#!/bin/bash
echo $1 $2 $3

LOCAL_DIR=/Users/mehmet/Desktop
FOLDER=watson-profile # deep-scheduler # service-capacity # kubernetes_sim # sim_packing # q_sim # fun

EC2_MACHINE_IP_L=( "ec2-18-212-189-174.compute-1.amazonaws.com" "54.89.173.144" )

GCLOUD_VM_IP=35.225.170.158

if [ $1  = 'ssh' ]; then
  if [ $2 = 'd' ]; then
    if [ -z "$3" ]; then
      echo "which dell node? 1-32"
    else
      ssh mfa51@spring.rutgers.edu "rm ~/.Xauthority"
      if [ "$3" -lt "10" ]; then
        echo "sshing to dell0$3"
        ssh -YXA -t mfa51@spring.rutgers.edu ssh -YX dell0$3
      else
        echo "sshing to dell$3"
        ssh -YXA -t mfa51@spring.rutgers.edu ssh -YX dell$3
      fi
    fi
  elif [ $2 = 'a' ]; then
    if [ -z "$3" ]; then
      echo "Which one 0, 1?"
    else
      echo ${EC2_MACHINE_IP_L[$3]}
      # ssh -v -i /home/chronos/user/Downloads/amazon_1.pem ubuntu@${EC2_MACHINE_IP_L[$3]}
      ssh -v -i ~/Desktop/amazon-1.pem ubuntu@${EC2_MACHINE_IP_L[$3]}
    fi
  elif [ $2 = 'm' ]; then
    # ssh -A -t mfa51@spring.rutgers.edu ssh mfa51@amarel.hpc.rutgers.edu
    ssh mfa51@amarel.hpc.rutgers.edu
  elif [ $2 = 'w' ]; then
    ssh maktas@ah-maktas-l.dhcp.mathworks.com
  elif [ $2 = 'g' ]; then
    ssh -Y mehmet@$GCLOUD_VM_IP
  fi
elif [ $1  = 'fr' ]; then
  if [ -z $2 ]; then
    echo "which remote d, a ?"
  elif [ $2  = 'd' ]; then
    scp -r mfa51@spring.rutgers.edu:~/Desktop/$FOLDER ~/Downloads
  elif [ $2  = 'a' ]; then
    scp -i amazon_1.pem ubuntu@$EC2_MACHINE_IP:~/$FOLDER/*.png ~/Downloads
  elif [ $2  = 'm' ]; then
    # scp mfa51@amarel.hpc.rutgers.edu:~/deep-scheduler/loglearning_persist/eval_wmpi_redsmall.out ~/Desktop
    scp mfa51@amarel.hpc.rutgers.edu:~/.emacs.d/init.el ~/Desktop
  elif [ $2 = 'g' ]; then
    scp mehmet@$GCLOUD_VM_IP:~/.emacs.d/init.el ~/Desktop
  fi
elif [ $1  = 'fs' ]; then
  if [ $2  = 'a' ]; then
    # IDENTITY_DIR=/home/chronos/user/Downloads/amazon-1.pem
    IDENTITY_DIR=$HOME/Desktop/amazon-1.pem
    REMOTE_DIR=ubuntu@${EC2_MACHINE_IP_L[$3]}:/home/ubuntu
    if [ $3  = 0 ]; then
      sshfs -o IdentityFile=$IDENTITY_DIR $REMOTE_DIR/$FOLDER $LOCAL_DIR/$FOLDER # -o debug,sshfs_debug,loglevel=debug
    elif [ $3  = 1 ]; then
      sshfs -o IdentityFile=$IDENTITY_DIR -o nonempty $REMOTE_DIR/shape_of_scale $LOCAL_DIR/shape_of_scale
    fi
  elif [ $2  = 'm' ]; then
    # IDENTITY_DIR=/home/chronos/user/.ssh/id_rsa.pub
    REMOTE_DIR=mfa51@amarel.hpc.rutgers.edu:/home/mfa51
    # ssh -f mfa51@spring.rutgers.edu -L 2222:amarel.hpc.rutgers.edu:22 -N
    # sshfs -p 2222 mfa51@127.0.0.1:/home/mfa51/$FOLDER $LOCAL_DIR/$FOLDER -o nonempty
    sshfs $REMOTE_DIR/$FOLDER $LOCAL_DIR/$FOLDER
  fi
elif [ $1  = 'um' ]; then
  umount $LOCAL_DIR/$FOLDER
elif [ $1  = 'tr' ]; then
  if [ -z $2 ]; then
    echo "which remote; a?"
  elif [ $2  = 'a' ]; then
    scp -i amazon_1.pem -r ~/Downloads/$FOLDER ubuntu@$EC2_MACHINE_IP:~/
  elif [ $2  = 's' ]; then
    scp -r ~/Downloads/sim_packing mfa51@spring.rutgers.edu:~/
  elif [ $2  = 'm' ]; then
    scp -r ~/.emacs.d/elpa/spacemacs* mfa51@amarel.hpc.rutgers.edu:~/.emacs.d/elpa
    # scp -r ~/.emacs.d/lisp mfa51@amarel.hpc.rutgers.edu:~/.emacs.d/lisp
  elif [ $2 = 'g' ]; then
    DIR=.emacs.d/themes # .emacs.d/elpa # .emacs.d/lisp
    scp -r ~/$DIR mehmet@$GCLOUD_VM_IP:~/.emacs.d
  fi
else
  echo "Argument did not match !"
fi
