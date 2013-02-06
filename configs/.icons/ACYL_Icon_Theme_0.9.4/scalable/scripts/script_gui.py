#!/usr/bin/env python


try:
	import gconf
except:
	print "You do not have the gconf module installed, however it is optional."

try:
	import gtk
	import gtk.glade
	import sys, os
	import subprocess
	import ConfigParser
	import shutil
	import gettext
	import locale
except:
	print "Some dependencies are missing!"
	print "The following ones are required:"
	print "gtk, gtk.glade, sys, os, subprocess,"
	print "shutil, gettext, ConfigParser"
	sys.exit(1)

#for arg in sys.argv:
#	print arg

class ACYLGUI:
	def __init__(self):

		APP = 'acyl'
		DIR = 'locale'

		locale.setlocale(locale.LC_ALL, '')
		for module in (gettext, gtk.glade):
			module.bindtextdomain(APP, DIR)
			module.textdomain(APP)
		_ = gettext.gettext

		self.widgetTree = gtk.glade.XML("gui.glade", None, APP)
		self.loadState("./current_state/gui_config.ini")
		self.widgetTree.get_widget("combobox1").set_active(0)
		self.CurrentSettings = str()
		self.colorset = str()
		self.gradset = str()
		self.filtset = str()
		self.codeset = str()

		if self.NbrColors == 1:
			self.widgetTree.get_widget("button6").set_sensitive(False)

		if self.NbrColors == 100:
			self.widgetTree.get_widget("button5").set_sensitive(False)

		self.FilterChange(self)

		if self.widgetTree.get_widget("combobox2").get_active() >= 1:
			self.widgetTree.get_widget("frame11").hide()
			self.widgetTree.get_widget("frame12").hide()
			self.widgetTree.get_widget("frame35").hide()
			self.widgetTree.get_widget("frame8").show()
			self.widgetTree.get_widget("frame9").show()
			self.widgetTree.get_widget("frame10").show()
		else:
			self.widgetTree.get_widget("frame8").hide()
			self.widgetTree.get_widget("frame9").hide()
			self.widgetTree.get_widget("frame10").hide()
			self.widgetTree.get_widget("frame11").show()
			self.widgetTree.get_widget("frame12").show()
			self.widgetTree.get_widget("frame35").show()

		dic = { \
			"on_close" : self.quit, \
			"on_notebook1_switch_page" : self.pageChange, \
			"on_combobox2_changed" : self.GradientChange, \
			"on_combobox3_changed" : self.FilterChange, \
			"on_folder_changed" : self.FolderChange, \
			"on_nav_changed" : self.navigationChange, \
			"on_logo_changed" : self.LogoChange, \
			"on_preset_change" : self.changePreset, \
			"on_custom_change" : self.CustomChange, \
			"on_import_from_graphic" : self.importGraphic, \
			"on_refresh" : self.RefreshPreview, \
			"on_apply_clicked" : self.ApplySettings, \
			"on_apply_icon_switch" : self.switchIcons, \
			"on_root_copy" : self.copyToRoot, \
			"on_make_exec" : self.makeExec, \
			"on_app_copy" : self.appCopy, \
			"on_about" : self.showAbout, \
			"on_import_settings" : self.importSettings, \
			"on_export_settings" : self.exportSettings, \
			"on_restore_settings" : self.restoreSettings, \
			"on_button5_clicked" : self.AddColor, \
			"on_button6_clicked" : self.RemoveColor \
			}

		self.widgetTree.signal_autoconnect(dic)

	def pageChange(self, widget, page, pagenum):
		if pagenum == 0:
			self.widgetTree.get_widget("button1").set_sensitive(True)
			self.widgetTree.get_widget("button3").set_sensitive(True)
			self.widgetTree.get_widget("combobox1").set_sensitive(True)
		if pagenum == 1:
			self.widgetTree.get_widget("button1").set_sensitive(True)
			self.widgetTree.get_widget("button3").set_sensitive(True)
			self.widgetTree.get_widget("combobox1").set_sensitive(True)
		if pagenum == 2:
			self.widgetTree.get_widget("button1").set_sensitive(False)
			self.widgetTree.get_widget("button3").set_sensitive(True)
			self.widgetTree.get_widget("combobox1").set_sensitive(False)
		if pagenum == 3:
			self.widgetTree.get_widget("button1").set_sensitive(False)
			self.widgetTree.get_widget("button3").set_sensitive(False)
			self.widgetTree.get_widget("combobox1").set_sensitive(False)

	def AddColor(self, widget):
		self.NbrColors = self.NbrColors + 1
		self.widgetTree.get_widget("colorbox" + str(self.NbrColors)).show()
		self.widgetTree.get_widget("button6").set_sensitive(True)
		if self.widgetTree.get_widget("checkbuttonOffset").get_active():
			if self.NbrColors > 1:
				step = (1.0 / (self.NbrColors-1))
				for i in range(0, self.NbrColors):
					self.widgetTree.get_widget("offsetbutton" + str(i+1)).set_value(step * i)
			else:
				self.widgetTree.get_widget("offsetbutton1").set_value(0);
		if self.NbrColors == 100:
			self.widgetTree.get_widget("button5").set_sensitive(False)
		self.RefreshPreview(self)

	def RemoveColor(self, widget):
		self.widgetTree.get_widget("colorbox" + str(self.NbrColors)).hide()
		self.NbrColors = self.NbrColors - 1
		self.widgetTree.get_widget("button5").set_sensitive(True)
		if self.widgetTree.get_widget("checkbuttonOffset").get_active():
			if self.NbrColors > 1:
				step = (1.0 / (self.NbrColors-1))
				for i in range(0, self.NbrColors):
					self.widgetTree.get_widget("offsetbutton" + str(i+1)).set_value(step * i)
			else:
				self.widgetTree.get_widget("offsetbutton1").set_value(0);
		if self.NbrColors == 1:
			self.widgetTree.get_widget("button6").set_sensitive(False)
		self.RefreshPreview(self)

	def ApplySettings(self, widget):
		self.RefreshPreview(self)
		page = self.widgetTree.get_widget("notebook1").get_current_page()
		if page <= 1:
			target = self.widgetTree.get_widget("combobox1").get_active()
			if target == 0:

				process = subprocess.Popen("source ./script_functions && apply_all " + self.CurrentSettings, shell=True, executable="/bin/bash", stdin=subprocess.PIPE, stdout=subprocess.PIPE)
				process.wait()
				self.setTheme(self)
				self.RefreshPreview(self)
			elif target == 1:
				self.showApplyCutom(self)
		elif page == 2:
			self.switchIcons(self)

	def setTheme(self, widget):
		try:
			client = gconf.client_get_default ()
			client.unset ("/desktop/gnome/interface/icon_theme")
			client.set_string ("/desktop/gnome/interface/icon_theme","")
			client.set_string ("/desktop/gnome/interface/icon_theme", "ACYL_Icon_Theme_0.9.4")
		except:
			print "The python gconf module is requierd to automatically set the icon-theme to this system."
		try:
			process = subprocess.Popen("killall gnome-panel", shell=True, executable="/bin/bash", stdin=subprocess.PIPE, stdout=subprocess.PIPE)
		except:
			print "Gnome-panel restart failed."

	def GradientChange(self, widget):
		if self.widgetTree.get_widget("combobox2").get_active() >= 1:
			self.widgetTree.get_widget("frame11").hide()
			self.widgetTree.get_widget("frame12").hide()
			self.widgetTree.get_widget("frame8").show()
			self.widgetTree.get_widget("frame9").show()
			self.widgetTree.get_widget("frame10").show()
			self.widgetTree.get_widget("frame35").hide()
		else:
			self.widgetTree.get_widget("frame35").show()
			self.widgetTree.get_widget("frame8").hide()
			self.widgetTree.get_widget("frame9").hide()
			self.widgetTree.get_widget("frame10").hide()
			self.widgetTree.get_widget("frame11").show()
			self.widgetTree.get_widget("frame12").show()

	def FilterChange(self, widget):
		if self.widgetTree.get_widget("combobox3").get_active() == 3:
			self.widgetTree.get_widget("filterbutton1").show()
			self.widgetTree.get_widget("filterbutton2").hide()
			self.widgetTree.get_widget("filterbutton3").hide()
		elif self.widgetTree.get_widget("combobox3").get_active() == 7:
			self.widgetTree.get_widget("filterbutton1").show()
			self.widgetTree.get_widget("filterbutton2").hide()
			self.widgetTree.get_widget("filterbutton3").hide()
		elif self.widgetTree.get_widget("combobox3").get_active() == 10:
			self.widgetTree.get_widget("filterbutton1").show()
			self.widgetTree.get_widget("filterbutton2").hide()
			self.widgetTree.get_widget("filterbutton3").hide()
		else:
			self.widgetTree.get_widget("filterbutton1").hide()
			self.widgetTree.get_widget("filterbutton2").hide()
			self.widgetTree.get_widget("filterbutton3").hide()

	def RefreshPreview(self, widget):
		page = self.widgetTree.get_widget("notebook1").get_current_page()
		if page == 0:
			self.makeSettingsGraphic(self)
		elif page == 1:
			self.makeSettingsCode(self)
		if page <= 1:
			process = subprocess.Popen("source ./script_functions && apply_preview " + self.CurrentSettings, shell=True, executable="/bin/bash", stdin=subprocess.PIPE, stdout=subprocess.PIPE)
			process.wait()
			self.saveState("./current_state/gui_config.ini")
			self.widgetTree.get_widget("image1").set_from_file("./preview_icons/apps.svg")
			self.widgetTree.get_widget("image2").set_from_file("./preview_icons/actions.svg")
			self.widgetTree.get_widget("image3").set_from_file("./preview_icons/emblems.svg")

	def importGraphic(self, widget):
		self.makeSettingsGraphic(self)
		self.widgetTree.get_widget("textview1").get_buffer().set_text(self.colorset + self.gradset + self.filtset)

	def makeSettingsCode(self, widget):
		start = self.widgetTree.get_widget("textview1").get_buffer().get_start_iter()
		end = self.widgetTree.get_widget("textview1").get_buffer().get_end_iter()
		self.codeset = self.widgetTree.get_widget("textview1").get_buffer().get_text(start, end, True)
		self.CurrentSettings ='''\''''+ str(self.codeset) + '''\''''

	def makeSettingsGraphic(self, widget):
		colorset = '''<linearGradient
     id=\"linearGradient4321\">'''

		for i in range(1, self.NbrColors + 1):
			color = "#" + self.getColor("colorbutton" + str(i))
			alpha = self.widgetTree.get_widget("colorbutton" + str(i)).get_alpha()/65535.0
			offset = self.widgetTree.get_widget("offsetbutton" + str(i)).get_value()
			#print "Color: " + color + "   Alpha: " + str(alpha) + "   Offset: " + str(offset)
			
			colorset = colorset + '''\n    <stop
       style=\"stop-color:''' + color + ''';stop-opacity:''' + str(alpha) + ''';\"
       offset=\"''' + str(offset) + '''\"
       id=\"stop''' + str(i) + '''\" />'''


		self.colorset = colorset + '''\n  </linearGradient>'''

		if self.widgetTree.get_widget("combobox2").get_active() == 1:
			r = self.widgetTree.get_widget("spinbutton-radius").get_value()
			cx = self.widgetTree.get_widget("spinbutton-center-x").get_value()
			cy = self.widgetTree.get_widget("spinbutton-center-y").get_value()
			fx = self.widgetTree.get_widget("spinbutton-focus-x").get_value()
			fy = self.widgetTree.get_widget("spinbutton-focus-y").get_value()

			self.gradset = '''<radialGradient
       inkscape:collect=\"always\"
       xlink:href=\"#linearGradient4321\"
       id=\"acyl_gradient\"
       cx=\"''' + str(cx) + '''%\"
       cy=\"''' + str(cy) + '''%\"
       fx=\\"''' + str(fx) + '''%\"
       fy="''' + str(fy) + '''%\"
       r=\"''' + str(r) + '''%\"
/>'''
		else:
			sx = self.widgetTree.get_widget("spinbutton-start-x").get_value()
			sy = self.widgetTree.get_widget("spinbutton-start-y").get_value()
			ex = self.widgetTree.get_widget("spinbutton-end-x").get_value()
			ey = self.widgetTree.get_widget("spinbutton-end-y").get_value()

			self.gradset = '''<linearGradient
     inkscape:collect=\"always\"
     xlink:href=\"#linearGradient4321\"
     id=\"acyl_gradient\"
     x1=\"''' + str(sx) + '''%\"
     y1=\"''' + str(sy) + '''%\"
     x2=\"''' + str(ex) + '''%\"
     y2=\"''' + str(ey) + '''%\" />'''

		if self.widgetTree.get_widget("combobox3").get_active()==0:filtset = 'disabled'
		if self.widgetTree.get_widget("combobox3").get_active()==1:filtset = 'wood'
		if self.widgetTree.get_widget("combobox3").get_active()==2:filtset = 'glass'
		if self.widgetTree.get_widget("combobox3").get_active()==3:
			alpha = self.widgetTree.get_widget("filterbutton1").get_alpha()/65535.0
			color = self.getColor("filterbutton1")
			filtset = 'paper \#' + str(color) + ' ' + str(alpha)
		if self.widgetTree.get_widget("combobox3").get_active()==4:filtset = 'cutout'
		if self.widgetTree.get_widget("combobox3").get_active()==5:filtset = 'brushed_metal'
		if self.widgetTree.get_widget("combobox3").get_active()==6:filtset = 'flame'
		if self.widgetTree.get_widget("combobox3").get_active()==7:
			alpha = self.widgetTree.get_widget("filterbutton1").get_alpha()/65535.0
			color = self.getColor("filterbutton1")
			filtset = 'quadratic \#' + str(color) + ' ' + str(alpha)
		if self.widgetTree.get_widget("combobox3").get_active()==8:filtset = 'bevel'
		if self.widgetTree.get_widget("combobox3").get_active()==9:filtset = 'outline'
		if self.widgetTree.get_widget("combobox3").get_active()==10:
			alpha = self.widgetTree.get_widget("filterbutton1").get_alpha()/65535.0
			color = self.getColor("filterbutton1")
			filtset = 'orb \#' + str(color) + ' ' + str(alpha)
		if self.widgetTree.get_widget("combobox3").get_active()==11:filtset = 'grungy'
		if self.widgetTree.get_widget("combobox3").get_active()==12:filtset = 'splatter'

		temp1 = subprocess.Popen('''source ./script_functions && get_filter var ''' + str(filtset) +''' && eval "echo -e '$var'"''', shell=True, executable="/bin/bash", stdin=subprocess.PIPE, stdout=subprocess.PIPE)

		temp2 = str(temp1.communicate()[0])
		self.filtset = temp2[:-1]

		self.CurrentSettings ='''\''''+ str(self.colorset) + '''\' \'''' + str(self.gradset) + '''\' \'''' + str(self.filtset) + '''\''''

	def getColor(self, button):
		color = self.widgetTree.get_widget(button).get_color()
		red = color.red*255/65535
		green = color.green*255/65535
		blue = color.blue*255/65535
		red = ("%X" % red).zfill(2)
		green = ("%X" % green).zfill(2)
		blue = ("%X" % blue).zfill(2)
		color = red + green + blue
		return str(color)

	def getAlpha(self, button):
		alpha = self.widgetTree.get_widget(button).get_alpha()/65535.0
		return str(alpha)

	def loadState(self, setfile):
		cp = ConfigParser.ConfigParser()
		cp.read(setfile)
		self.NbrColors = int(cp.get("state-graphicview", "nbrcol"))
		if self.NbrColors <= 100:
			self.widgetTree.get_widget("button6").set_sensitive(True)
		if self.NbrColors >= 1:
			self.widgetTree.get_widget("button5").set_sensitive(True)
		if self.NbrColors == 100:
			self.widgetTree.get_widget("button5").set_sensitive(False)
		if self.NbrColors == 1:
			self.widgetTree.get_widget("button6").set_sensitive(False)
		colorlist = cp.get("state-graphicview", "colors").split("', '")
		alphalist = cp.get("state-graphicview", "alphas").split("', '")
		offsetlist = cp.get("state-graphicview", "offsets").split(", ")
		if self.NbrColors != 1:
			colorlist[0] = str(colorlist[0])[-6:]
			colorlist[-1] = str(colorlist[-1])[:-2]
			alphalist[0] = str(alphalist[0])[2:]
			alphalist[-1] = str(alphalist[-1])[:-2]
			offsetlist[0] = str(offsetlist[0])[1:]
			offsetlist[-1] = str(offsetlist[-1])[:-1]
		else:
			colorlist[0] = str(colorlist[0])[2:-2]
			alphalist[0] = str(alphalist[0])[2:-2]
			offsetlist[0] = str(offsetlist[0])[1:-1]
		for i in range(1, self.NbrColors + 1):
			color = colorlist[i - 1]
			alpha = int(float(alphalist[i - 1])*65535)
			offset = float(offsetlist[i - 1])
			self.widgetTree.get_widget("colorbutton" + str(i)).set_color(gtk.gdk.Color('#' + color))
			self.widgetTree.get_widget("colorbutton" + str(i)).set_alpha(alpha)
			self.widgetTree.get_widget("offsetbutton" + str(i)).set_value(offset)
			self.widgetTree.get_widget("colorbox" + str(i)).show()
		for i in range(self.NbrColors + 1, 101):
			self.widgetTree.get_widget("colorbutton" + str(i)).set_color(gtk.gdk.Color('#000000'))
			self.widgetTree.get_widget("colorbutton" + str(i)).set_alpha(int(65535))
			self.widgetTree.get_widget("offsetbutton" + str(i)).set_value(int(1.0))
			self.widgetTree.get_widget("colorbox" + str(i)).hide()

		self.widgetTree.get_widget("spinbutton-radius").set_value(float(cp.get("state-graphicview", "radius")))
		self.widgetTree.get_widget("spinbutton-center-x").set_value(float(cp.get("state-graphicview", "center-x")))
		self.widgetTree.get_widget("spinbutton-center-y").set_value(float(cp.get("state-graphicview", "center-y")))
		self.widgetTree.get_widget("spinbutton-focus-x").set_value(float(cp.get("state-graphicview", "focus-x")))
		self.widgetTree.get_widget("spinbutton-focus-y").set_value(float(cp.get("state-graphicview", "focus-y")))
		self.widgetTree.get_widget("spinbutton-start-x").set_value(float(cp.get("state-graphicview", "start-x")))
		self.widgetTree.get_widget("spinbutton-start-y").set_value(float(cp.get("state-graphicview", "start-y")))
		self.widgetTree.get_widget("spinbutton-end-x").set_value(float(cp.get("state-graphicview", "end-x")))
		self.widgetTree.get_widget("spinbutton-end-y").set_value(float(cp.get("state-graphicview", "end-y")))
		self.widgetTree.get_widget("combobox2").set_active(int(cp.get("state-graphicview", "gradtype")))
		self.widgetTree.get_widget("combobox3").set_active(int(cp.get("state-graphicview", "filter")))
		color = cp.get("state-graphicview", "filtercolor1")
		alpha = int(float(cp.get("state-graphicview", "filteralpha1"))*65535)
		self.widgetTree.get_widget("filterbutton1").set_color(gtk.gdk.Color('#' + color))
		self.widgetTree.get_widget("filterbutton1").set_alpha(alpha)
		color = cp.get("state-graphicview", "filtercolor2")
		alpha = int(float(cp.get("state-graphicview", "filteralpha2"))*65535)
		self.widgetTree.get_widget("filterbutton2").set_color(gtk.gdk.Color('#' + color))
		self.widgetTree.get_widget("filterbutton2").set_alpha(alpha)
		color = cp.get("state-graphicview", "filtercolor3")
		alpha = int(float(cp.get("state-graphicview", "filteralpha3"))*65535)
		self.widgetTree.get_widget("filterbutton3").set_color(gtk.gdk.Color('#' + color))
		self.widgetTree.get_widget("filterbutton3").set_alpha(alpha)
		self.widgetTree.get_widget("textview1").get_buffer().set_text(cp.get("state-codeview", "settings"))
		self.widgetTree.get_widget("combobox6").set_active(int(cp.get("state-switchicons", "logo")))
		self.widgetTree.get_widget("combobox5").set_active(int(cp.get("state-switchicons", "navigation")))
		self.widgetTree.get_widget("combobox4").set_active(int(cp.get("state-switchicons", "folder")))

	def saveState(self, setfile):
		cp = ConfigParser.ConfigParser()
		cp.read(setfile)
		colorlist = []
		alphalist = []
		offsetlist = []
		for i in range(1, self.NbrColors + 1):
			colorlist.append(self.getColor("colorbutton" + str(i)))	
			alphalist.append(self.getAlpha("colorbutton" + str(i)))	
			offsetlist.append(self.widgetTree.get_widget("offsetbutton" + str(i)).get_value())	
		cp.set("state-graphicview", "colors", colorlist)
		cp.set("state-graphicview", "alphas", alphalist)
		cp.set("state-graphicview", "offsets", offsetlist)
		cp.set("state-graphicview", "nbrcol", self.NbrColors)
		cp.set("state-graphicview", "filtercolor1", self.getColor("filterbutton1"))
		cp.set("state-graphicview", "filtercolor2", self.getColor("filterbutton2"))
		cp.set("state-graphicview", "filtercolor3", self.getColor("filterbutton3"))
		cp.set("state-graphicview", "filteralpha1", self.getAlpha("filterbutton1"))
		cp.set("state-graphicview", "filteralpha2", self.getAlpha("filterbutton2"))
		cp.set("state-graphicview", "filteralpha3", self.getAlpha("filterbutton3"))
		cp.set("state-graphicview", "radius", self.widgetTree.get_widget("spinbutton-radius").get_value())
		cp.set("state-graphicview", "center-x", self.widgetTree.get_widget("spinbutton-center-x").get_value())
		cp.set("state-graphicview", "center-y", self.widgetTree.get_widget("spinbutton-center-y").get_value())
		cp.set("state-graphicview", "focus-x", self.widgetTree.get_widget("spinbutton-focus-x").get_value())
		cp.set("state-graphicview", "focus-y", self.widgetTree.get_widget("spinbutton-focus-y").get_value())
		cp.set("state-graphicview", "start-x", self.widgetTree.get_widget("spinbutton-start-x").get_value())
		cp.set("state-graphicview", "start-y", self.widgetTree.get_widget("spinbutton-start-y").get_value())
		cp.set("state-graphicview", "end-x", self.widgetTree.get_widget("spinbutton-end-x").get_value())
		cp.set("state-graphicview", "end-y", self.widgetTree.get_widget("spinbutton-end-y").get_value())
		cp.set("state-graphicview", "gradtype", self.widgetTree.get_widget("combobox2").get_active())
		cp.set("state-graphicview", "filter", self.widgetTree.get_widget("combobox3").get_active())
		start = self.widgetTree.get_widget("textview1").get_buffer().get_start_iter()
		end = self.widgetTree.get_widget("textview1").get_buffer().get_end_iter()
		cp.set("state-codeview", "settings", self.widgetTree.get_widget("textview1").get_buffer().get_text(start, end, True))
		cp.set("state-switchicons", "folder", self.widgetTree.get_widget("combobox4").get_active())
		cp.set("state-switchicons", "navigation", self.widgetTree.get_widget("combobox5").get_active())
		cp.set("state-switchicons", "logo", self.widgetTree.get_widget("combobox6").get_active())
		cp.write(open(setfile, "w"))

	def FolderChange(self, widget):
		temp = self.widgetTree.get_widget("combobox4").get_active()
		self.folder = "acyl_" + str(int(temp) + 1)
		self.widgetTree.get_widget("image4").set_from_file("../alternative_icons/folders/"+ self.folder +"/places/folder.svg")

	def LogoChange(self, widget):
		temp = self.widgetTree.get_widget("combobox6").get_active()
		if temp == 0:
			self.logo = "arch"
			self.widgetTree.get_widget("filechooserbutton1").hide()
		elif temp == 1:
			self.logo = "debian"
			self.widgetTree.get_widget("filechooserbutton1").hide()
		elif temp == 2:
			self.logo = "fedora"
			self.widgetTree.get_widget("filechooserbutton1").hide()
		elif temp == 3:
			self.logo = "gnome"
			self.widgetTree.get_widget("filechooserbutton1").hide()
		elif temp == 4:
			self.logo = "ubuntu"
			self.widgetTree.get_widget("filechooserbutton1").hide()
		elif temp == 5:
			self.logo = "zenwalk"
			self.widgetTree.get_widget("filechooserbutton1").hide()
		elif temp == 6:
			self.logo = "gentoo"
			self.widgetTree.get_widget("filechooserbutton1").hide()
		elif temp == 7:
			self.logo = "bodhi"
			self.widgetTree.get_widget("filechooserbutton1").show()
		elif temp == 8:
			self.logo = "custom"
			self.widgetTree.get_widget("filechooserbutton1").show()
		self.widgetTree.get_widget("image5").set_from_file("../alternative_icons/logos/" + self.logo)

	def CustomChange(self, widget):
		custom = str(self.widgetTree.get_widget("filechooserbutton1").get_filename())
		process = subprocess.Popen("source ./script_functions && set_custom_logo " + custom, shell=True, executable="/bin/bash", stdin=subprocess.PIPE, stdout=subprocess.PIPE)
		process.wait()
		self.widgetTree.get_widget("image5").set_from_file("../alternative_icons/logos/custom")

	def navigationChange(self, widget):
		tempn = self.widgetTree.get_widget("combobox5").get_active()
		if tempn == 0:
			self.nav = "default"
		elif tempn == 1:
			self.nav = "moblin"
		elif tempn == 2:
			self.nav = "minimal"
		self.widgetTree.get_widget("image6").set_from_file("../alternative_icons/navigation/"+ self.nav +"/back.svg")

	def switchIcons(self, widget):
		tempf = self.widgetTree.get_widget("combobox4").get_active()
		self.folder = "acyl_" + str(int(tempf) + 1)
		temp = self.widgetTree.get_widget("combobox6").get_active()
		if temp == 0:
			self.logo = "arch"
		elif temp == 1:
			self.logo = "debian"
		elif temp == 2:
			self.logo = "fedora"
		elif temp == 3:
			self.logo = "gnome"
		elif temp == 4:
			self.logo = "ubuntu"
		elif temp == 5:
			self.logo = "zenwalk"
		elif temp == 6:
			self.logo = "gentoo"
		elif temp == 7:
			self.logo = "custom"
		tempn = self.widgetTree.get_widget("combobox5").get_active()
		if tempn == 0:
			self.nav = "default"
		elif tempn == 1:
			self.nav = "moblin"
		elif tempn == 2:
			self.nav = "minimal"
		process = subprocess.Popen("source ./script_functions && logo_change " + self.logo, shell=True, executable="/bin/bash", stdin=subprocess.PIPE, stdout=subprocess.PIPE)
		process = subprocess.Popen("source ./script_functions && folder_change " + self.folder, shell=True, executable="/bin/bash", stdin=subprocess.PIPE, stdout=subprocess.PIPE)
		process = subprocess.Popen("source ./script_functions && navigation_change " + self.nav, shell=True, executable="/bin/bash", stdin=subprocess.PIPE, stdout=subprocess.PIPE)
		process.wait()
		self.saveState("./current_state/gui_config.ini")
		self.setTheme(self)

	def copyToRoot(self, widget):
		pwd = self.widgetTree.get_widget("entry1").get_text()
		if len(pwd) > 0:
			process = subprocess.Popen("source ./script_functions && root_copy " + pwd, shell=True, executable="/bin/bash", stdin=subprocess.PIPE, stdout=subprocess.PIPE)
			process.wait()
		else:
			self.askPwd(self)

	def makeExec(self, widget):
		pwd = self.widgetTree.get_widget("entry1").get_text()
		if len(pwd) > 0:
			process = subprocess.Popen("source ./script_functions && make_exec " + pwd, shell=True, executable="/bin/bash", stdin=subprocess.PIPE, stdout=subprocess.PIPE)
			process.wait()
		else:
			self.askPwd(self)

	def changePreset(self, widget):
		temp = self.widgetTree.get_widget("combobox9").get_active()
		if temp == 0:
			self.widgetTree.get_widget("spinbutton-start-x").set_value(50)
			self.widgetTree.get_widget("spinbutton-start-y").set_value(0)
			self.widgetTree.get_widget("spinbutton-end-x").set_value(50)
			self.widgetTree.get_widget("spinbutton-end-y").set_value(100)
		elif temp == 1:
			self.widgetTree.get_widget("spinbutton-start-x").set_value(0)
			self.widgetTree.get_widget("spinbutton-start-y").set_value(50)
			self.widgetTree.get_widget("spinbutton-end-x").set_value(100)
			self.widgetTree.get_widget("spinbutton-end-y").set_value(50)
		else:
			self.widgetTree.get_widget("spinbutton-start-x").set_value(0)
			self.widgetTree.get_widget("spinbutton-start-y").set_value(0)
			self.widgetTree.get_widget("spinbutton-end-x").set_value(100)
			self.widgetTree.get_widget("spinbutton-end-y").set_value(100)

	def appCopy(self, widget):
		pwd = self.widgetTree.get_widget("entry1").get_text()
		if len(pwd) > 0:
			active = self.widgetTree.get_widget("combobox8").get_active()
			if active != -1:
				if active == 0:
					app = "pidgin"
				elif active == 1:
					app = "sonata"
				elif active == 2:
					app = "emesene"
				elif active == 3:
					app = "wicd"
				process = subprocess.Popen("source ./script_functions && " + app + "_copy " + pwd, shell=True, executable="/bin/bash", stdin=subprocess.PIPE, stdout=subprocess.PIPE)
				process.wait()
		else:
			self.askPwd(self)

	def importSettings(self, widget):
		dialog = gtk.FileChooserDialog("Open file", None, gtk.FILE_CHOOSER_ACTION_OPEN, (gtk.STOCK_CANCEL, gtk.RESPONSE_CANCEL, gtk.STOCK_OPEN, gtk.RESPONSE_OK))
		dialog.set_default_response(gtk.RESPONSE_OK)
		dialog.set_default_response(gtk.RESPONSE_OK)

		filter = gtk.FileFilter()
		filter.set_name("All files")
		filter.add_pattern("*")
		dialog.add_filter(filter)

		response = dialog.run()
		if response == gtk.RESPONSE_OK:
			self.loadState(dialog.get_filename())
			self.RefreshPreview(self)
		dialog.destroy()

	def exportSettings(self, widget):
		dialog = gtk.FileChooserDialog("Save file", None, gtk.FILE_CHOOSER_ACTION_SAVE, (gtk.STOCK_CANCEL, gtk.RESPONSE_CANCEL, gtk.STOCK_SAVE, gtk.RESPONSE_OK))
		dialog.set_default_response(gtk.RESPONSE_OK)
		dialog.set_default_response(gtk.RESPONSE_OK)
		dialog.set_do_overwrite_confirmation(True)
		filter = gtk.FileFilter()
		filter.set_name("All files")
		filter.add_pattern("*")
		dialog.add_filter(filter)

		response = dialog.run()
		if response == gtk.RESPONSE_OK:
			src = "./current_state/gui_config.ini"
			dest = dialog.get_filename()
			shutil.copyfile(src, dest)
			self.RefreshPreview(self)
		dialog.destroy()

	def restoreSettings(self, widget):
		self.loadState("./gui_config_default.ini")
		self.RefreshPreview(self)

	def quit(self, widget):
		self.saveState("./current_state/gui_config.ini")
		gtk.main_quit()

####### Dialog1 #######
	def showDialog(self, widget):
		self.dialog = gtk.glade.XML("gui.glade", "window2")
		dialog_dic = { \
		"on_close"  : self.closeDialog \
		}
		self.dialog.signal_autoconnect(dialog_dic)
		self.mdialog = self.dialog.get_widget("window2")
		self.widgetTree.get_widget("button15").set_sensitive(False)
		self.widgetTree.get_widget("button16").set_sensitive(False)
		self.widgetTree.get_widget("button17").set_sensitive(False)
		self.widgetTree.get_widget("combobox8").set_sensitive(False)
		self.mdialog.show()

	def closeDialog(self, widget=None, data=None):
		self.mdialog.destroy()

####### askPwd #######
	def askPwd(self, widget):
		self.dialog = gtk.glade.XML("gui.glade", "window2")
		dialog_dic = { \
		"on_ok" : self.dialogOk \
		}
		self.dialog.signal_autoconnect(dialog_dic)
		self.pwdDialog = self.dialog.get_widget("window2")
		self.pwdDialog.show()

	def dialogOk(self, widget=None, data=None):
		self.pwdDialog.destroy()

####### AboutDialog1 #######
	def showAbout(self, widget):
		self.dialog = gtk.glade.XML("gui.glade", "aboutdialog1")
		dialog_dic = { \
		"on_close_dialog"  : self.closeDialog \
		}
		self.dialog.signal_autoconnect(dialog_dic)
		self.mdialog = self.dialog.get_widget("aboutdialog1")
		self.mdialog.show()

	def closeDialog(self, widget=None, data=None):
		self.mdialog.destroy()
####### ApplyCutom  #######
	def showApplyCutom(self, widget):
		self.dialog = gtk.glade.XML("gui.glade", "ApplyCustom")
		dialog_dic = { \
		"on_apply_custom" : self.applyCustom, \
		"on_close_dialog"  : self.closeDialog \
		}
		self.dialog.signal_autoconnect(dialog_dic)
		self.mdialog = self.dialog.get_widget("ApplyCustom")
		self.mdialog.show()

	def closeDialog(self, widget=None, data=None):
		self.mdialog.destroy()

	def applyCustom(self, widget=None, data=None):
		self.RefreshPreview(self)
		if self.dialog.get_widget("checkbutton1").get_active():
			process = subprocess.Popen("source ./script_functions && apply_actions " + self.CurrentSettings, shell=True, executable="/bin/bash", stdin=subprocess.PIPE, stdout=subprocess.PIPE)
			process.wait()
		if self.dialog.get_widget("checkbutton2").get_active():
			process = subprocess.Popen("source ./script_functions && apply_emblems " + self.CurrentSettings, shell=True, executable="/bin/bash", stdin=subprocess.PIPE, stdout=subprocess.PIPE)
			process.wait()
		if self.dialog.get_widget("checkbutton3").get_active():
			process = subprocess.Popen("source ./script_functions && apply_places " + self.CurrentSettings, shell=True, executable="/bin/bash", stdin=subprocess.PIPE, stdout=subprocess.PIPE)
			process.wait()
		if self.dialog.get_widget("checkbutton4").get_active():
			process = subprocess.Popen("source ./script_functions && apply_mimetype " + self.CurrentSettings, shell=True, executable="/bin/bash", stdin=subprocess.PIPE, stdout=subprocess.PIPE)
			process.wait()
		if self.dialog.get_widget("checkbutton5").get_active():
			process = subprocess.Popen("source ./script_functions && apply_status " + self.CurrentSettings, shell=True, executable="/bin/bash", stdin=subprocess.PIPE, stdout=subprocess.PIPE)
			process.wait()
		if self.dialog.get_widget("checkbutton6").get_active():
			process = subprocess.Popen("source ./script_functions && apply_apps " + self.CurrentSettings, shell=True, executable="/bin/bash", stdin=subprocess.PIPE, stdout=subprocess.PIPE)
			process.wait()
		if self.dialog.get_widget("checkbutton7").get_active():
			process = subprocess.Popen("source ./script_functions && apply_except " + self.CurrentSettings, shell=True, executable="/bin/bash", stdin=subprocess.PIPE, stdout=subprocess.PIPE)
			process.wait()
		self.setTheme(self)
		self.mdialog.destroy()


	def main(self):
		gtk.main()

if __name__ == "__main__":
	acyl = ACYLGUI()
	acyl.main()

