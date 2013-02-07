#!/bin/bash

RCFILE=".AwOkenrcWhite"
CONFIG_FILE="$HOME/$RCFILE"
VER="2.4"
ICNST="AwOkenWhite"

clear
echo "*************************************************************************"
echo "CONFIGURATION SCRIPT - ZENITY INSTALLED"
echo " "

if [ $1 ]; then
	ICONSDIR=$1
	echo "I'm using this path for iconset in configuration process: $ICONSDIR"
else
	cd ../
	ICONSDIR=`pwd`
	echo "I'm using this path for iconset in configuration process: $ICONSDIR"
fi

echo ""

cp $ICONSDIR/$RCFILE $CONFIG_FILE

source $CONFIG_FILE
#echo "Now I'll modify the configuration file located in $CONFIG_FILE."
echo "*************************************************************************"

#############################################################################
#                                   FOLDER TYPE                             #
#############################################################################
TARGET_KEY="folder_type"
TIPO=`zenity --title="$ICNST $VER Customization script" --list --text="What kind of folder type do you prefer?\t\t\t\t\t\t" --radiolist --column "" --column "" FALSE "awoken" FALSE "classy" FALSE "leaf" FALSE "metal" FALSE "original" FALSE "s11" FALSE "s11-original" FALSE "snowsabre" FALSE "sonetto" FALSE "tlag" FALSE "token" FALSE "See icons before choosing them" --width="350" --height="400"`

#************************************************************************
# If number of choice doesn't exist, print an error and re-ask the question:
while [ "$TIPO" = "" ]; do
  zenity --warning --text="\nERROR: You selected nothing\!\nSelect a proper option." --title="ERROR"
  TIPO=`zenity --title="$ICNST $VER Customization script" --list --text="What kind of folder type do you prefer?\t\t\t\t\t\t" --radiolist --column "" --column "" FALSE "awoken" FALSE "classy" FALSE "leaf" FALSE "metal" FALSE "original" FALSE "s11" FALSE "s11-original" FALSE "snowsabre" FALSE "sonetto" FALSE "tlag" FALSE "token" FALSE "See icons before choosing them" --width="350" --height="400"`
done

#************************************************************************
# If you chose to see icons, open folder and re-ask the question:
while [ "$TIPO" = "See icons before choosing them" ]; do
  xdg-open $ICONSDIR/clear/128x128/places &
  TIPO=`zenity --title="$ICNST $VER Customization script" --list --text="What kind of folder type do you prefer?\t\t\t\t\t\t" --radiolist --column "" --column "" FALSE "awoken" FALSE "classy" FALSE "leaf" FALSE "metal" FALSE "original" FALSE "s11" FALSE "s11-original" FALSE "snowsabre" FALSE "sonetto" FALSE "tlag" FALSE "token" FALSE "See icons before choosing them" --width="350" --height="400"`
done

sed -i "s/\($TARGET_KEY *=*\).*/\1'$TIPO'/" $CONFIG_FILE
echo -e "Your $TARGET_KEY choice was:\t\t$TIPO"

TARGET_KEY="folder_sub_type"	
if [ $TIPO = "awoken" ] || [ $TIPO = "classy" ] || [ $TIPO = "leaf" ] || [ $TIPO = "original" ] || [ $TIPO = "s11" ] || [ $TIPO = "s11-original" ] || [ $TIPO = "snowsabre" ] || [ $TIPO = "sonetto" ] || [ $TIPO = "tlag" ]; then

  QST="Your choice was $TIPO. There are different sub-types for this option.\nChoose the one from the list below:"
   	  
  case $TIPO in
  awoken)
    SUBQST="FALSE awokenclear FALSE awokenwhite FALSE awokendark FALSE cherryblossom FALSE grass FALSE kaki FALSE ribes FALSE sand FALSE sky FALSE violet"
  ;;
  classy)
    SUBQST="FALSE aluminum FALSE black-gray FALSE blue FALSE ocean FALSE steel"
  ;;
  leaf)
    SUBQST="FALSE blue FALSE green FALSE red FALSE white"
  ;;
  original)
    SUBQST="FALSE dark FALSE gray FALSE violet FALSE white"
  ;;
  s11*)
    SUBQST="FALSE normal FALSE blue FALSE green FALSE gray FALSE orange FALSE purple FALSE red FALSE yellow"
  ;;
  snowsabre)
    SUBQST="FALSE black FALSE silver"
  ;;
  sonetto)
    SUBQST="FALSE normal FALSE sonetto01 FALSE sonetto02 FALSE sonetto03 FALSE sonetto04 FALSE sonetto05 FALSE sonetto06 FALSE sonetto07 FALSE sonetto08 FALSE sonetto09 FALSE sonetto10"
  ;;
  tlag)
    SUBQST="FALSE tlagdark FALSE tlaglight"
  ;;
  esac
  
  SOTTOTIPO=`zenity --title="$ICNST $VER Customization script" --list --text="$QST" --radiolist --column "" --column "" $SUBQST --width="410" --height="353"`
  
  while [ "$SOTTOTIPO" = "" ]; do
    zenity --warning --text="\nERROR: You selected nothing\!\nSelect a proper option." --title="ERROR"
    SOTTOTIPO=`zenity --title="$ICNST $VER Customization script" --list --text="$QST" --radiolist --column "" --column "" $SUBQST --width="410" --height="353"`
  done
else
  SOTTOTIPO="none"
fi

echo -e "Your $TARGET_KEY choice was:\t$SOTTOTIPO"
sed -i "s/\($TARGET_KEY *=*\).*/\1'$SOTTOTIPO'/" $CONFIG_FILE

#############################################################################
#                                   START HERE                              #
#############################################################################
TARGET_KEY="start_here"
QST="What kind of start here icon do you prefer?\nI'll not show all options, but if you want I'll open a window to see them (if xdg-open is supported).\nDo you want this?"
RISP=`zenity --title="$ICNST $VER Customization script" --list --text="$QST" --radiolist --column "" --column "" FALSE "Yes" TRUE "No" --width="410" --height="250"`

if [ "$RISP" = "Yes" ]; then
  xdg-open $ICONSDIR/clear/128x128/start-here &
fi

TIPO=`zenity --title="$ICNST $VER Customization script" --entry --text="Now type the name of the icon of choice.\nWARNING: type only the last part of the icon before file extension!!\nFor example, type 'gaia2' or 'centos' or 'arch4':"`

while [ ! -f "$ICONSDIR/clear/128x128/start-here/start-here-$TIPO.png" ]; do
    zenity --warning --text="\nERROR: $TIPO is not valid, I'm afraid. Type a correct option." --title="ERROR"
	echo "$TIPO is not valid, I'm afraid. Type a correct option."
    TIPO=`zenity --title="$ICNST $VER Customization script" --entry --text="Now type the name of the icon of choice.\nWARNING: type only the last part of the icon before file extension!! For example, type 'gaia2' or 'centos' or 'arch4':"`
done		

echo -e "Your $TARGET_KEY choice was:\t\t$TIPO"
sed -i "s/\($TARGET_KEY *=*\).*/\1'$TIPO'/" $CONFIG_FILE

#############################################################################
#                                 TRASH TYPE                                #
#############################################################################
TARGET_KEY="trash_type"
QST="What kind of trash icon do you prefer?"
TIPO=`zenity --title="$ICNST $VER Customization script" --list --text="$QST" --radiolist --column "" --column "" FALSE "trash1" FALSE "trash2" FALSE "trash3" FALSE "trash4" TRUE "See icons before choosing them" --width="300" --height="250"`

while [ "$TIPO" = "See icons before choosing them" ] || [ "$TIPO" = "" ]; do
  if [ "$TIPO" = "See icons before choosing them" ]; then
    PERCORSO=$ICONSDIR/clear/128x128/places
    `xdg-mime query default image/png | cut -d. -f1 ` $PERCORSO/user-trash1.png $PERCORSO/user-trash1-full.png $PERCORSO/user-trash2.png $PERCORSO/user-trash2-full.png $PERCORSO/user-trash3.png $PERCORSO/user-trash3-full.png $PERCORSO/user-trash4.png $PERCORSO/user-trash4-full.png &
  elif [ "$TIPO" = "" ]; then
    zenity --warning --text="\nERROR: You selected nothing\!\nSelect a proper option." --title="ERROR"
    echo "You selected nothing! Select a proper option."
  fi
  TIPO=`zenity --title="$ICNST $VER Customization script" --list --text="$QST" --radiolist --column "" --column "" FALSE "trash1" FALSE "trash2" FALSE "trash3" FALSE "trash4" TRUE "See icons before choosing them" --width="300" --height="250"`
done

echo -e "Your $TARGET_KEY choice was:\t\t$TIPO"
sed -i "s/\($TARGET_KEY *=*\).*/\1'$TIPO'/" $CONFIG_FILE

#############################################################################
#                              COLOR - NOCOLOR                              #
#############################################################################
TARGET_KEY="color_type"
QST="Do you want colored applications or greyish apps?"
TIPO=`zenity --title="$ICNST $VER Customization script" --list --text="$QST" --radiolist --column "" --column "" FALSE "color" FALSE "no-color" --width="300" --height="250"`

while [ "$TIPO" = "" ]; do
  zenity --warning --text="\nERROR: You selected nothing\!\nSelect a proper option." --title="ERROR"
  echo "You selected nothing! Select a proper option."
  TIPO=`zenity --title="$ICNST $VER Customization script" --list --text="$QST" --radiolist --column "" --column "" FALSE "color" FALSE "no-color" --width="300" --height="150"`
done

echo -e "Your $TARGET_KEY choice was:\t\t$TIPO"
sed -i "s/\($TARGET_KEY *=*\).*/\1'$TIPO'/" $CONFIG_FILE

#############################################################################
#                                  COMPUTER                                 #
#############################################################################
TARGET_KEY="computer_type"
QST="What kind of computer icon do you prefer?"
TIPO=`zenity --title="$ICNST $VER Customization script" --list --text="$QST" --radiolist --column "" --column "" FALSE "computer1" FALSE "computer2" FALSE "computer3" FALSE "computer4" FALSE "computer5" TRUE "See icons before choosing them" --width="300" --height="250"`

while [ "$TIPO" = "See icons before choosing them" ] || [ "$TIPO" = "" ]; do
  if [ "$TIPO" = "See icons before choosing them" ]; then
	PERCORSO=$ICONSDIR/clear/128x128/places
	`xdg-mime query default image/png | cut -d. -f1 ` $PERCORSO/user-desktop1.png $PERCORSO/user-desktop2.png $PERCORSO/user-desktop3.png $PERCORSO/user-desktop4.png $PERCORSO/user-desktop5.png &
  elif [ "$TIPO" = "" ]; then
    zenity --warning --text="\nERROR: You selected nothing\!\nSelect a proper option." --title="ERROR"
    echo "You selected nothing! Select a proper option."
  fi
  TIPO=`zenity --title="$ICNST $VER Customization script" --list --text="$QST" --radiolist --column "" --column "" FALSE "computer1" FALSE "computer2" FALSE "computer3" FALSE "computer4" FALSE "computer5" TRUE "See icons before choosing them" --width="300" --height="250"`
done

echo -e "Your $TARGET_KEY choice was:\t\t$TIPO"
sed -i "s/\($TARGET_KEY *=*\).*/\1'$TIPO'/" $CONFIG_FILE

#############################################################################
#                                    HOME                                   #
#############################################################################
TARGET_KEY="home_type"
QST="What kind of home icon do you prefer?"
TIPO=`zenity --title="$ICNST $VER Customization script" --list --text="$QST" --radiolist --column "" --column "" FALSE "home1" FALSE "home2" FALSE "home3" FALSE "home4" FALSE "home5" TRUE "See icons before choosing them" --width="300" --height="250"`

while [ "$TIPO" = "See icons before choosing them" ] || [ "$TIPO" = "" ]; do
  if [ "$TIPO" = "See icons before choosing them" ]; then
	PERCORSO=$ICONSDIR/clear/128x128/places
	`xdg-mime query default image/png | cut -d. -f1 ` $PERCORSO/user-home1.png $PERCORSO/user-home2.png $PERCORSO/user-home3.png $PERCORSO/user-home4.png $PERCORSO/user-home5.png &
  elif [ "$TIPO" = "" ]; then
    zenity --warning --text="\nERROR: You selected nothing\!\nSelect a proper option." --title="ERROR"
    echo "You selected nothing! Select a proper option."
  fi
  TIPO=`zenity --title="$ICNST $VER Customization script" --list --text="$QST" --radiolist --column "" --column "" FALSE "home1" FALSE "home2" FALSE "home3" FALSE "home4" FALSE "home5" TRUE "See icons before choosing them" --width="300" --height="250"`
done

echo -e "Your $TARGET_KEY choice was:\t\t$TIPO"
sed -i "s/\($TARGET_KEY *=*\).*/\1'$TIPO'/" $CONFIG_FILE

#############################################################################
#                                    END                                    #
#############################################################################
echo "*************************************************************************"
echo "Configuration process is finished."
zenity --info --text="Configuration process is finished.\nNow I'll start the recovery script.\n\nClick the button below to continue."

bash $ICONSDIR/extra/recover.sh $ICONSDIR
