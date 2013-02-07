#!/bin/bash

RCFILE=".AwOkenrcWhite"
CONFIG_FILE="$HOME/$RCFILE"
VER="2.4"
ICNST="AwOkenWhite"

clear
echo "*************************************************************************"
echo "CONFIGURATION SCRIPT"
echo " "

if [ $1 ]; then
	ICONSDIR=$1
	echo "I'm using this path for iconset in configuration process: $ICONSDIR"
else
	cd ../
	ICONSDIR=`pwd`
	echo "I'm using this path for iconset in configuration process: $ICONSDIR"
fi

echo "*************************************************************************"

cp $ICONSDIR/$RCFILE $CONFIG_FILE
source $CONFIG_FILE
echo "Now I'll modify the configuration file located in $CONFIG_FILE."
echo "*************************************************************************"
echo "What kind of folder type do you prefer? (Type the number of your choice)" 
TARGET_KEY="folder_type"
PS3="$TARGET_KEY? "
select TIPO in "awoken" "classy" "leaf" "metal" "original" "s11" "s11-original" "snowsabre" "sonetto" "tlag" "token" "See icons before choosing them"
do
	if [ "$TIPO" = "" ]; then
		echo "Wrong choice!"
    elif [ "$TIPO" = "See icons before choosing them" ]; then
        CARTELLE="1) awoken\t\t\t 7) s11-original\n2) classy\t\t\t 8) snowsabre\n3) leaf\t\t\t\t 9) sonetto\n4) metal\t\t\t10) tlag\n5) original\t\t\t11) token\n6) s11\t\t\t\t12) See icons before choosing them"
        echo -e $CARTELLE
        xdg-open $ICONSDIR/clear/128x128/places &
	else
		sed -i "s/\($TARGET_KEY *=*\).*/\1'$TIPO'/" $CONFIG_FILE
		TARGET_KEY="folder_sub_type"
		
	    if [ $TIPO = "awoken" ] || [ $TIPO = "classy" ] || [ $TIPO = "leaf" ] || [ $TIPO = "original" ] || [ $TIPO = "s11" ] || [ $TIPO = "s11-original" ] || [ $TIPO = "snowsabre" ] || [ $TIPO = "sonetto" ] || [ $TIPO = "tlag" ]; then
	    
          echo "Your choice was $TIPO. There are different sub-types for this option. Choose the one from the list below:"
    	  PS3="Wich folder do you prefer? "	
    	  
	      case $TIPO in
	      awoken)
        	select SOTTOTIPO in "awokenclear" "awokenwhite" "awokendark" "cherryblossom" "grass" "kaki" "ribes" "sand" "sky" "violet"; do
        	  if [ "$SOTTOTIPO" = "" ]; then
		        echo "Wrong choice! You have to type the number of your choice."
	          else
    		    echo "Your choice was: $SOTTOTIPO"
		        break
	          fi
          done
	      ;;
	      classy)
        	select SOTTOTIPO in "aluminum" "black-gray" "blue" "ocean" "steel"; do
        	  if [ "$SOTTOTIPO" = "" ]; then
		        echo "Wrong choice! You have to type the number of your choice."
	          else
    		    echo "Your choice was: $SOTTOTIPO"
		        break
	          fi
          done
	      ;;
	      leaf)
        	select SOTTOTIPO in "blue"  "green"  "red"  "white"; do
        	  if [ "$SOTTOTIPO" = "" ]; then
		        echo "Wrong choice! You have to type the number of your choice."
	          else
    		    echo "Your choice was: $SOTTOTIPO"
		        break
	          fi
          done
	      ;;
	      original)
        	select SOTTOTIPO in "dark"  "gray"  "violet"  "white"; do
        	  if [ "$SOTTOTIPO" = "" ]; then
		        echo "Wrong choice! You have to type the number of your choice."
	          else
    		    echo "Your choice was: $SOTTOTIPO"
		        break
	          fi
          done
	      ;;
	      s11*)
        	select SOTTOTIPO in "normal" "blue" "green" "gray" "orange" "purple" "red" "yellow"; do
        	  if [ "$SOTTOTIPO" = "" ]; then
		        echo "Wrong choice! You have to type the number of your choice."
	          else
    		    echo "Your choice was: $SOTTOTIPO"
		        break
	          fi
          done
	      ;;
	      snowsabre)
        	select SOTTOTIPO in "black" "silver"; do
        	  if [ "$SOTTOTIPO" = "" ]; then
		        echo "Wrong choice! You have to type the number of your choice."
	          else
    		    echo "Your choice was: $SOTTOTIPO"
		        break
	          fi
          done
	      ;;
	      sonetto)
        	select SOTTOTIPO in "normal" "sonetto01" "sonetto02" "sonetto03" "sonetto04" "sonetto05" "sonetto06" "sonetto07" "sonetto08" "sonetto09" "sonetto10"; do
        	  if [ "$SOTTOTIPO" = "" ]; then
		        echo "Wrong choice! You have to type the number of your choice."
	          else
    		    echo "Your choice was: $SOTTOTIPO"
		        break
	          fi
          done
	      ;;  
	      tlag)
        	select SOTTOTIPO in "tlagdark" "tlaglight"; do
        	  if [ "$SOTTOTIPO" = "" ]; then
		        echo "Wrong choice! You have to type the number of your choice."
	          else
    		    echo "Your choice was: $SOTTOTIPO"
		        break
	          fi
          done
	      ;; 
	      esac
	    
	    else
		  echo "Your choice was: $TIPO"
	      SOTTOTIPO="none"
		fi
		
    	sed -i "s/\($TARGET_KEY *=*\).*/\1'$SOTTOTIPO'/" $CONFIG_FILE
    	break
	fi
done

echo "*************************************************************************"
echo "What kind of start here icon do you prefer? I'll not show all 90 options, but if you want I'll open a window to see them (if xdg-open is supported). Do you want this (y/[n])?"
read RISP
if [ $RISP = "y" ]; then
	xdg-open $ICONSDIR/clear/128x128/start-here &
fi
TARGET_KEY="start_here"
PS3="$TARGET_KEY? "
echo "Now type the name of the icon of choice."
echo "WARNING: type only the last part of the icon before file extension!! For example, type 'gaia2' or 'centos' or 'arch4'"
read -p $PS3 TIPO
while [ ! -f "$ICONSDIR/clear/128x128/start-here/start-here-$TIPO.png" ]; do
	echo "$TIPO is not valid, I'm afraid. Type a correct option"
	read -p $PS3 TIPO
done		
echo "Your choice was $TIPO"
sed -i "s/\($TARGET_KEY *=*\).*/\1'$TIPO'/" $CONFIG_FILE
echo "*************************************************************************"
echo "What kind of trash icon do you prefer? (Type the number of your choice)"
TARGET_KEY="trash_type"
PS3="$TARGET_KEY? "
select TIPO in "trash1" "trash2" "trash3" "trash4" "See icons before choosing them"
do
	if [ "$TIPO" = "" ]; then
		echo "Wrong choice!"
    elif [ "$TIPO" = "See icons before choosing them" ]; then
        PERCORSO=$ICONSDIR/clear/128x128/places
        `xdg-mime query default image/png | cut -d. -f1 ` $PERCORSO/user-trash1.png $PERCORSO/user-trash1-full.png $PERCORSO/user-trash2.png $PERCORSO/user-trash2-full.png $PERCORSO/user-trash3.png $PERCORSO/user-trash3-full.png $PERCORSO/user-trash4.png $PERCORSO/user-trash4-full.png &
	else
		echo "Your choice was: $TIPO"
		sed -i "s/\($TARGET_KEY *=*\).*/\1'$TIPO'/" $CONFIG_FILE
		break
	fi
done
echo "*************************************************************************"
echo "Do you want colored applications or greyish apps? (Type the number of your choice)"
TARGET_KEY="color_type"
PS3="$TARGET_KEY? "
select TIPO in "color" "no-color"
do
	if [ "$TIPO" = "" ]; then
		echo "Wrong choice!"
	else
		echo "Your choice was: $TIPO"
		sed -i "s/\($TARGET_KEY *=*\).*/\1'$TIPO'/" $CONFIG_FILE
		break
	fi
done
echo "*************************************************************************"
echo "Last two questions (I incorporate them in one question): change home icon and computer icon."
echo "For each option, type the number of your choice, as above. Name that ends with 1 is the colored version, name that ends with 2 is the grayish version."
TARGET_KEY="computer_type"
PS3="$TARGET_KEY? "
select TIPO in "computer1" "computer2" "computer3" "computer4" "computer5" "See icons before choosing them"
do
	if [ "$TIPO" = "" ]; then
		echo "Wrong choice!"
    elif [ "$TIPO" = "See icons before choosing them" ]; then
        PERCORSO=$ICONSDIR/clear/128x128/places
        `xdg-mime query default image/png | cut -d. -f1 ` $PERCORSO/user-desktop1.png $PERCORSO/user-desktop2.png $PERCORSO/user-desktop3.png $PERCORSO/user-desktop4.png $PERCORSO/user-desktop5.png &
	else
		echo "Your choice was: $TIPO"
		sed -i "s/\($TARGET_KEY *=*\).*/\1'$TIPO'/" $CONFIG_FILE
		break
	fi
done
TARGET_KEY="home_type"
PS3="$TARGET_KEY? "
select TIPO in "home1" "home2" "home3" "home4" "home5" "See icons before choosing them"
do
	if [ "$TIPO" = "" ]; then
		echo "Wrong choice!"
    elif [ "$TIPO" = "See icons before choosing them" ]; then
        PERCORSO=$ICONSDIR/clear/128x128/places
        `xdg-mime query default image/png | cut -d. -f1 ` $PERCORSO/user-home1.png $PERCORSO/user-home2.png $PERCORSO/user-home3.png $PERCORSO/user-home4.png $PERCORSO/user-home5.png &
	else
		echo "Your choice was: $TIPO"
		sed -i "s/\($TARGET_KEY *=*\).*/\1'$TIPO'/" $CONFIG_FILE
		break
	fi
done
echo "*************************************************************************"
echo "Configuration process is finished. Now I'll start the recovery script. Type any key to continue."
read CONTINUE
bash $ICONSDIR/extra/recover.sh $ICONSDIR
