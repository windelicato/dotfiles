#!/bin/bash

CONFIG_FILE="$HOME/.AwOkenrcWhite"
ICONSET="AwOkenWhite"
HOMEDIR="$HOME/.icons"
ICONSDIR="${@: -1}"

FOLDERTYPE=""
FOLDERSUBTYPE=""

##########################################################################################
# SCRIPT OPTIONS:                                                                        #
#   -F -> Folder type (it requires folder-sub-type option to work, but no check is done) #
#   -f -> Folder sub-type                                                                #
#   -S -> start-here                                                                     #
#   -T -> trash type                                                                     #
#   -G -> gedit type                                                                     #
#   -C -> computer type                                                                  #
#   -H -> home type                                                                      #
#   -c -> color/no-color                                                                 #
#                                                                                        #
# WARNING: Remember to always call "-c" option first, otherwise "-G", "-C", "-H" could   #
#          not work properly!                                                            #
##########################################################################################

##########################################################################################
# USEFUL FUNCTIONS:                                                                      #
##########################################################################################
usage()
{
cat << EOF
usage: $0 options

This script actually configure the iconset.

OPTIONS:
   -F -> Folder type (it requires folder sub-type option to work, but no check is done)
   -f -> Folder sub-type                                                                
   -S -> start-here                                                                     
   -T -> trash type                                                                     
   -C -> computer type                                                                  
   -H -> home type                                                                      
   -c -> color/no-color 
   
WARNING: Remember to always call "-c" option first, otherwise "-G", "-C", "-H" could not work properly!
EOF
}
##########################################################################################
color()
{
		for dim in 128x128 24x24; do
		  echo $dim
  		cd clear/$dim/actions
	  	for i in *1.png; do
	  		if [ $i != viewmag1.png ] && [ $i != stock_zoom-1.png ]; then
	  			ln -fs $i ${i%1.*}.png
	  		fi
	  	done
	  	
	  	cd ../apps
	  	for i in *1.png; do
	  		if [ $i != "config-date1.png" ] && [ $i != "glippy1.png" ] && [ $i != "it.vodafone*" ] && [ $i != "nm-stage01-connecting01.png" ] && [ $i != "nm-stage01-connecting11.png" ] && [ $i != "nm-stage02-connecting01.png" ] && [ $i != "nm-stage02-connecting11.png" ] && [ $i != "nm-stage03-connecting01.png" ] && [ $i != "nm-stage03-connecting11.png" ] && [ $i != "ubuntuone-client1.png" ]; then
	  			ln -fs $i ${i%1.*}.png
	  		fi
	  	done
	  	cd ../
	  	
	  	for fld in categories devices; do
  	  	cd $fld
	    	for i in *1.png; do
	    		ln -fs $i ${i%1.*}.png
	    	done
	    	cd ../
	  	done
	  	
	  	cd places
	  	for i in *1.png; do
	  		if [ $i != folder-linux1.png ] && [ $i != user-trash1.png ]; then
	  			ln -fs $i ${i%1.*}.png
	  		fi
	  	done
	  	cd ../../../
    done
    
    cd clear/22x22/actions
    echo 22x22
		for i in *1.png; do
  		if [ $i != viewmag1.png ] && [ $i != stock_zoom-1.png ]; then
  			ln -fs $i ${i%1.*}.png
  		fi
  	done
  	cd ../../../
}
##########################################################################################
nocolor()
{
		for dim in 128x128 24x24; do
		  echo $dim
  		cd clear/$dim/
		
	  	for fld in actions categories devices; do
  	  	cd $fld
  	  	for i in *2.png; do
	  		  if [ $i != playlist-layouts-22.png ]; then
	  			  ln -fs $i ${i%2.*}.png
	  		  fi
	    	done
	    	cd ../
      done
	  	
	  	cd apps
	  	for i in *2.png; do
	  		if [ $i != "wincloser32.png" ] && [ $i != "texmaker32x32.png" ] && [ $i != "texmaker22x22.png" ] && [ $i != "quake2.png" ] && [ $i != "netbeans2.png" ] && [ $i != "glade-2.png" ] && [ $i != "gnome-robots2.png" ] && [ $i != "kexi-2.png" ] && [ $i != "kexi2.png" ] && [ $i != "glade2.png" ] && [ $i != "gnobots2.png" ] && [ $i != "config-date2.png" ] && [ $i != "blueclock32.png" ] && [ $i != "glippy2.png" ] && [ $i != "it.vodafone*" ] && [ $i != "nm-stage01-connecting02.png" ] && [ $i != "nm-stage01-connecting12.png" ] && [ $i != "nm-stage02-connecting02.png" ] && [ $i != "nm-stage02-connecting12.png" ] && [ $i != "nm-stage03-connecting02.png" ] && [ $i != "nm-stage03-connecting12.png" ] && [ $i != "ubuntuone-client2.png" ] && [ $i != "onboard2.png" ] && [ $i != "kmail2.png" ] && [ $i != "control-center2.png" ] && [ $i != "control-center2.xpm" ]; then
	  			ln -fs $i ${i%2.*}.png
	  		fi
	  	done
	  	cd ../
	  	
	  	cd places
	  	for i in *2.png; do
	  		if [ $i != folder-linux2.png ] && [ $i != user-trash2.png ]; then
	  			ln -fs $i ${i%2.*}.png
	  		fi
	  	done
	  	cd ../../../
		done
		
    cd clear/22x22/actions
    echo 22x22
		for i in *2.png; do
  			ln -fs $i ${i%2.*}.png
  	done
  	cd ../../../
}
##########################################################################################
changefolder()
{
  TYPE=$1
  SUBTYPE=$2
  ICNST=$3
  ICONSDIR=$4
  HOMEDIR="$HOME/.icons/$ICNST"
  
  if [ "$TYPE" = "leaf" ] || [ "$TYPE" = "snowsabre" ] || [ "$TYPE" = "classy" ] || [ "$TYPE" = "awoken" ] || [ "$TYPE" = "original" ] || [ "$TYPE" = "tlag" ]; then
	  TYPE="$TYPE/$SUBTYPE"
  fi
  
  if [ "$TYPE" = "s11" ]; then
	  TYPE="s11/$TYPE"
  fi

  cd $ICONSDIR/clear/128x128/places/$TYPE/

  for f in *; do
	  cd $HOMEDIR/clear/128x128/places
  	ln -fs $TYPE/$f $f
  	cd $ICONSDIR/clear/128x128/places/$TYPE/
  done

  cd $ICONSDIR/clear/24x24/places/$TYPE/

  for f in *; do
  	cd $HOMEDIR/clear/24x24/places
  	ln -fs $TYPE/$f $f
  	cd $ICONSDIR/clear/24x24/places/$TYPE/
  done

  if [ "$TYPE" = "s11/s11" ] || [ "$TYPE" = "s11/s11-original" ]; then
    cd $HOMEDIR/clear/128x128/places/$TYPE
  	ln -fs ../s11-folders/$SUBTYPE".png" "folder.png"
  	cd $HOMEDIR/clear/24x24/places/$TYPE
  	ln -fs ../s11-folders/$SUBTYPE".png" "folder.png"
  fi

  if [ "$TYPE" = "sonetto" ]; then
		cd $HOMEDIR/clear/128x128/places/$TYPE
		ln -fs folder/$SUBTYPE".png" "folder.png"
		cd $HOMEDIR/clear/24x24/places/$TYPE
		ln -fs folder/$SUBTYPE".png" "folder.png"
  fi 
}
##########################################################################################

echo $ICONSDIR
echo "*************************************************************************"
echo "CUSTOMIZATION SCRIPT"

# Creating folder in $HOMEDIR directory if it doesn't exist                           #

if [ $ICONSDIR == "/usr/share/icons/$ICONSET" ];then
	if [ ! -d $HOMEDIR/$ICONSET ]; then
		cp -dpRf $ICONSDIR $HOMEDIR
	fi
fi

HOMEDIR=$HOMEDIR/$ICONSET
cd $HOMEDIR

while getopts "hF:f:S:T:G:C:H:c:" opt; do
  case $opt in
    h)
      usage
      exit 1
    ;;
    F)
      FOLDERTYPE=$OPTARG;;
    f)
      FOLDERSUBTYPE=$OPTARG;;
##########################################################################################
    S)
  		TARGET_KEY="start_here"
	  	echo "Changing start here logo to $OPTARG..."
	  	
  		cd clear/128x128/places
	  	ln -fs ../start-here/start-here-$OPTARG.png start-here.png
		  cd ../../../	
	
  		cd clear/24x24/places
	  	ln -fs ../start-here/start-here-$OPTARG.png start-here.png
		  cd ../../../	
		  
	   	echo "The set is changed according to the $TARGET_KEY $OPTARG option."
    ;;
##########################################################################################
    T)
	  	TARGET_KEY="trash_type"
  		echo "Changing trash icon to $OPTARG..."
  		
  		cd clear/128x128/places
	  	ln -fs user-$OPTARG.png user-trash.png
	  	ln -fs user-$OPTARG-full.png user-trash-full.png
	  	cd ../../../
	  	cd clear/24x24/places
	  	ln -fs user-$OPTARG.png user-trash.png
	  	ln -fs user-$OPTARG-full.png user-trash-full.png
	  	cd ../../../
	  	
	   	echo "The set is changed according to the $OPTARG option."
    ;;
##########################################################################################
    C)
	  	TARGET_KEY="computer_type"
	  	NOME=${OPTARG:8}
  
	  	echo "Changing computer icon to $OPTARG..."
  
	  	cd clear/128x128/places
	  	ln -fs user-desktop$NOME.png user-desktop.png
	  	cd ../../../
	  	cd clear/24x24/places
	  	ln -fs user-desktop$NOME.png user-desktop.png
	  	cd ../../../
	  	
	   	echo "The set is changed according to the $OPTARG option."
    ;;
##########################################################################################
    H)
	  	TARGET_KEY="home_type"
  
	  	echo "Changing home icon to $OPTARG..."
  
	  	cd clear/128x128/places
	  	ln -fs user-$OPTARG.png user-home.png
	  	cd ../../../
	  	cd clear/24x24/places
	  	ln -fs user-$OPTARG.png user-home.png
	  	cd ../../../
	  	
	   	echo "The set is changed according to the $OPTARG option."
    ;;
##########################################################################################
    c)
	  	TARGET_KEY="color_type"
	  	
  		echo "Changing color type to $OPTARG..."
  		
      if [ "$OPTARG" == "color" ]; then
        color
      elif [ "$OPTARG" == "no-color" ]; then
        nocolor
      fi
      
	   	echo "The set is changed according to the $OPTARG option."
    ;;
##########################################################################################
    \?)
      echo "Invalid option: -$OPTARG" >&2
      echo ""
      usage
      exit 1
      ;;
    :)
      echo "Option -$OPTARG requires an argument." >&2
      echo ""
      usage
      exit 1
      ;;
  esac
done

if [ "$FOLDERTYPE" != "" ] && [ "$FOLDERSUBTYPE" != "" ]; then
	TARGET_KEY="folder_type"
	echo "Changing folder type to $FOLDERTYPE $FOLDERSUBTYPE..."

	changefolder $FOLDERTYPE $FOLDERSUBTYPE $ICONSET $ICONSDIR
  
  echo "The set is changed according to the $TARGET_KEY $FOLDERTYPE $FOLDERSUBTYPE option."
elif [ [ "$FOLDERTYPE" == "" ] || [ "$FOLDERSUBTYPE" == "" ] ] && [ [ "$FOLDERTYPE" != "" ] && [ "$FOLDERSUBTYPE" != "" ] ]; then
  echo "WARNING: folder type and folder subtype needs to be coupled to get the script working.\nI'll not change folder icons."
fi

exit
