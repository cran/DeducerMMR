.makeNewMmrDialog <- function() {
	
	dialog <- new(SimpleRDialog)
	dialog$setSize(500L, 700L)
	dialog$setTitle("Moderated Multiple Regression")
	
	
	variableSelector <- new(VariableSelectorWidget)
	variableSelector$setTitle("data")
	addComponent(dialog, variableSelector, 10, 400, 500, 10)
	
	dependent <- new(SingleVariableWidget, variableSelector)
	dependent$setTitle("Dependent Variable", TRUE)
	addComponent(dialog, dependent, 10, 970, 110, 470)
	
	model <- new(J("DeducerPlugInMMR.VariableListWidgetExtended2"), 
		variableSelector)
	model$setTitle("Model", TRUE)
	addComponent(dialog, model, 115, 970, 510, 470)
	
	combo <- new(ButtonGroupWidget, c("Uncentered", "Mean Centered", 
		"Residual Centered"))
	combo$setTitle("Methods", TRUE)
	addComponent(dialog, combo, 520, 400, 690, 10)
	combo$setDefaultModel("Mean Centered")
	
	
	combo1 <- new(ButtonGroupWidget, c("Simultaneous Analysis", 
		"Hierarchical Analysis"))
	combo1$setTitle("Output", TRUE)
	addComponent(dialog, combo1, 695, 400, 810, 10)
	combo1$setDefaultModel("Hierarchical Analysis")
	
	
	
	JButton <- J("javax.swing.JButton")
	button <- new(JButton, "                            Simple Slopes Analysis                            ")
	addComponent(dialog, button, 865, 980, 915, 350, topType = "REL", 
		rightType = "REL", bottomType = "REL", leftType = "NONE")
	
	
	
	subDialog1 <- new(SimpleRSubDialog, dialog, "- Simple Slopes Analysis -")
	setSize(subDialog1, 550L, 500L)
	
	
	variableSelector1 <- new(VariableSelectorWidget)
	variableSelector1$setTitle("data1")
	addComponent(subDialog1, variableSelector1, 10, 320, 550, 
		10)
	
	predictor <- new(SingleVariableWidget, variableSelector1)
	predictor$setTitle("Predictor", TRUE)
	addComponent(subDialog1, predictor, 130, 970, 280, 390)
	
	moderator1 <- new(SingleVariableWidget, variableSelector1)
	moderator1$setTitle("Moderator 1", TRUE)
	addComponent(subDialog1, moderator1, 290, 970, 440, 390)
	
	moderator2 <- new(SingleVariableWidget, variableSelector1)
	moderator2$setTitle("Moderator 2 (only for three way interaction)", 
		TRUE)
	addComponent(subDialog1, moderator2, 450, 970, 600, 390)
	
	#addComponent(subDialog1, combo3, 560, 320, 700, 10)
	
	combo3 <- new(ButtonGroupWidget, c("None", "Print"))
	combo3$setTitle("Plot Slopes", TRUE)
	addComponent(subDialog1, combo3, 560, 320, 700, 10)
	combo3$setDefaultModel("None")
	
	JButton1 <- J("javax.swing.JButton")
	button1 <- new(JButton1, "Plot Options")
	addComponent(subDialog1, button1, 710, 320, 800, 10)
	
	subDialog2 <- new(SimpleRSubDialog, subDialog1, "- Plot Options -")
	setSize(subDialog2, 550L, 600L)
	
	#Listen for the button to be pressed
	ActionListener1 <- J("org.rosuda.deducer.widgets.event.RActionListener")
	actionFunction1 <- function(cmd, ActionEvent) {
		subDialog2$setLocationRelativeTo(button1)
		subDialog2$run()
	}
	listener1 <- new(ActionListener1)
	listener1$setFunction(toJava(actionFunction1))
	button1$addActionListener(listener1)
	
	
	xlabel <- new(TextFieldWidget, "X axis label")
	addComponent(subDialog2, xlabel, 10, 350, 100, 10)
	xlabel$setDefaultModel(c("Default"))
	xlabel$setInteger(FALSE)
	
	
	infx <- new(TextFieldWidget, "X min")
	addComponent(subDialog2, infx, 10, 770, 100, 700)
	infx$setNumeric(TRUE)
	
	
	supx <- new(TextFieldWidget, "X max")
	addComponent(subDialog2, supx, 10, 870, 100, 800)
	supx$setNumeric(TRUE)
	
	
	namelimx <- new(JLabel, "X axis limits:")
	addComponent(subDialog2, namelimx, 50, 690, 100, 570)
	
	
	ylabel <- new(TextFieldWidget, "Y axis label")
	addComponent(subDialog2, ylabel, 160, 350, 250, 10)
	ylabel$setDefaultModel(c("Default"))
	ylabel$setInteger(FALSE)
	
	
	infy <- new(TextFieldWidget, "Y min")
	addComponent(subDialog2, infy, 160, 770, 250, 700)
	infy$setNumeric(TRUE)
	
	
	supy <- new(TextFieldWidget, "Y max")
	addComponent(subDialog2, supy, 160, 870, 250, 800)
	supy$setNumeric(TRUE)
	
	
	namelimy <- new(JLabel, "Y axis limits:")
	addComponent(subDialog2, namelimy, 200, 690, 250, 570)
	
	#
	#pippo$setBorder(bordo)
	#addComponent(subDialog2, pippo, 350, 450, 820, 
	#\t10)
	
	labelSlope1 <- new(TextFieldWidget, "Label slope 1")
	addComponent(subDialog2, labelSlope1, 350, 450, 450, 10)
	labelSlope1$setDefaultModel(c("Default"))
	labelSlope1$setInteger(FALSE)
	
	labelSlope2 <- new(TextFieldWidget, "Label slope 2")
	addComponent(subDialog2, labelSlope2, 470, 450, 570, 10)
	labelSlope2$setDefaultModel(c("Default"))
	labelSlope2$setInteger(FALSE)
	
	labelSlope3 <- new(TextFieldWidget, "Label slope 3 (only for three-way intercations)")
	addComponent(subDialog2, labelSlope3, 590, 450, 690, 10)
	labelSlope3$setDefaultModel(c("Default"))
	labelSlope3$setInteger(FALSE)
	
	labelSlope4 <- new(TextFieldWidget, "Label slope 4 (only for three-way intercations)")
	addComponent(subDialog2, labelSlope4, 710, 450, 810, 10)
	labelSlope4$setDefaultModel(c("Default"))
	labelSlope4$setInteger(FALSE)
	
	
	
	#Listen for the button to be pressed
	ActionListener <- J("org.rosuda.deducer.widgets.event.RActionListener")
	actionFunction <- function(cmd, ActionEvent) {
		subDialog1$setLocationRelativeTo(button)
		subDialog1$run()
	}
	listener <- new(ActionListener)
	listener$setFunction(toJava(actionFunction))
	button$addActionListener(listener)
	
	
	
	coded <- new(J("DeducerPlugInMMR.VariableListWidgetExtended"), 
		variableSelector)
	coded$setTitle("Coded Variables", TRUE)
	addComponent(dialog, coded, 520, 970, 800, 470)
	
	
	
	
	#required library ###############################################
	status <- .deducer$requirePackage("pequod")
	if (status == "installed") {
		execute("library('pequod')")
	}
	else if (status == "not installed") {
		stop("package pequod required")
	}
	
	
	
	.MmrRunFunction <- function(state) {
		
		
		#print(state)
		#a print statement is useful for debugging
		
		
		dep <- state$`Dependent Variable`
		ind <- state$Model
		met <- state$Methods
		dat <- state$data
		code <- state$`Coded Variables`
		out <- state$Output
		pred <- state$Predictor
		mode1 <- state$`Moderator 1`
		mode2 <- state$`Moderator 2 (only for three way interaction)`
		plotSlope <- state$`Plot Slopes`
		xlab <- state$`X axis label`
		ylab <- state$`Y axis label`
		xmin <- state$`X min`
		xmax <- state$`X max`
		ymin <- state$`Y min`
		ymax <- state$`Y max`
		slLab1 <- state$`Label slope 1`
		slLab2 <- state$`Label slope 2`
		slLab3 <- state$`Label slope 3`
		slLab4 <- state$`Label slope 4`
		
		
		
		# model
		lungo <- length(ind)
		ind1 <- ind[1]
		
		if (lungo > 1) {
			for (i in (2:lungo)) {
				ind1 <- paste(ind1, "+", ind[i])
			}
		}
		
		
		
		
		# coded variables vector
		lungo1 <- length(code)
		
		if (lungo1 == 0) {
			
			cod = paste("\"", "none", "\"", sep = "")
		}
		
		if (lungo1 > 0) {
			ddf <- paste("\"", code[1], "\"", sep = "")
			if (lungo1 > 1) {
				for (i in (2:lungo1)) {
				ddf[i] <- paste("\"", code[i], "\"", sep = "")
				}
				ddf1 <- ddf[1]
				for (i in (2:lungo1)) {
				ddf1 <- paste(ddf1, ",", ddf[i])
				}
				
				cod <- paste("c(", ddf1, ")")
			}
			else {
				cod <- paste(ddf)
			}
		}
		
		#### MMR Uncentered ####
		if ("Uncentered" %in% met) {
			mod <- paste("modMMR<-lmres(", dep, "~", ind1, ",", 
				dat, ")")
		}
		
		
		#### MMR with mean centering ####
		if ("Mean Centered" %in% met) {
			
			modtemp <- paste("mod1<-lmres(", dep, "~", ind1, 
				",", dat, ");code<-", cod, ";nume<-numSelected(mod1,code)")
			
			mod <- paste("modMMR<-lmres(", dep, "~", ind1, ",", 
				dat, ",", "centered=nume)")
		}
		
		if ("Residual Centered" %in% met) {
			mod <- paste("modMMR<-lmres(", dep, "~", ind1, ",", 
				dat, ",", "residual_centering=TRUE)")
		}
		
		#### SIMPLE SLOPES #####
		if ((length(pred) + length(mode1)) > 0) {
			p <- ddf <- paste("\"", pred, "\"", sep = "")
			m1 <- ddf <- paste("\"", mode1, "\"", sep = "")
			
			if (length(mode2) == 0) {
				
				
				if (length(code) == 0) {
				cmd02 <- paste("Ssl<-simpleSlope(modMMR, pred=", 
					p, ",mod1=", m1, ")")
				}
				else {
				cmd02 <- paste("Ssl<-simpleSlope(modMMR, pred=", 
					p, ",mod1=", m1, ",coded=", cod, ")")
				}
			}
			
			if (length(mode2) > 0) {
				m2 <- ddf <- paste("\"", mode2, "\"", sep = "")
				if (length(code) == 0) {
				cmd02 <- paste("Ssl<-simpleSlope(modMMR, pred=", 
					p, ",mod1=", m1, ",mod2=", m2, ")")
				}
				else {
				cmd02 <- paste("Ssl<-simpleSlope(modMMR, pred=", 
					p, ",mod1=", m1, ",mod2=", m2, ",coded=", 
					cod, ")")
				}
			}
		}
		
		#### Plot #####
		if ("Print" %in% plotSlope) {
			
			if (length(xlab) == 0 | xlab == "Default") {
				NX = paste("\"", "default", "\"", sep = "")
			}
			else {
				NX = paste("\"", xlab, "\"", sep = "")
			}
			
			if (length(ylab) == 0 | ylab == "Default") {
				NY = paste("\"", "default", "\"", sep = "")
			}
			else {
				NY = paste("\"", ylab, "\"", sep = "")
			}
			
			if (xmin == "" | xmax == "") {
				LX = paste("\"", "default", "\"", sep = "")
			}
			else {
				LX = paste("c(", xmin, ",", xmax, ")")
			}
			
			if (ymin == "" | ymax == "") {
				LY = paste("\"", "default", "\"", sep = "")
			}
			else {
				LY = paste("c(", ymin, ",", ymax, ")")
			}
			
			if (length(slLab3) == 0 | slLab3 == "Default" | length(slLab4) == 
				0 | slLab4 == "Default") {
				if (length(slLab2) == 0 | slLab2 == "Default" | 
				length(slLab1) == 0 | slLab1 == "Default") {
				NM = paste("\"", "default", "\"", sep = "")
				}
				else {
				NM = paste("c(", "\"", slLab1, "\"", ",", "\"", 
					slLab2, "\"", ")", sep = "")
				}
			}
			else {
				NM = paste("c(", "\"", slLab1, "\"", ",", "\"", 
				slLab2, "\"", ",", "\"", slLab3, "\"", ",", 
				"\"", slLab4, "\"", ")", sep = "")
			}
			
			
			
			
			
			
			
			
			
			cmd07 <- paste("JavaGD(width=800, height=400, ps=12);plotSlopes<-PlotSlope(Ssl, namemod=", 
				NM, ",namex=", NX, ",namey=", NY, ",limitx=", 
				LX, ",limity=", LY, ");plotSlopes")
		}
		
		## STRING TITLE MMR AND SIMPLE SLOPES""
		cmdt2 <- paste("\"  ************ SIMPLE SLOPES ANALYSIS ************ \"")
		
		if ("Mean Centered" %in% met) {
			
			cmdt0 <- cat("\n", "\" ************************************************************ \"", 
				"\n", "\"  ** MODERATED MULTIPLE REGRESSION WITH CENTERED PREDICTORS ** \"", 
				"\n", "\"  ************************************************************ \"", 
				fill = TRUE)
			
			##
		}
		else {
			if ("Uncentered" %in% met) {
				cmdt0 <- cat("\n", "\" ************************************************************** \"", 
				"\n", "\"  ** MODERATED MULTIPLE REGRESSION WITH UNCENTERED PREDICTORS ** \"", 
				"\n", "\"  ************************************************************** \"", 
				fill = TRUE)
			}
			if ("Residual Centered" %in% met) {
				cmdt0 <- cat("\n", "\" *********************************************************** \"", 
				"\n", "\"  ** MODERATED MULTIPLE REGRESSION WITH RESIDUAL CENTERING ** \"", 
				"\n", "\"  *********************************************************** \"", 
				fill = TRUE)
			}
			
			
			##
		}
		
		
		
		#### OUTPUT #####
		
		if ("Print" %in% plotSlope) {
			
			if ("Mean Centered" %in% met) {
				
				
				cmd0 <- paste(modtemp)
				
				if ("Simultaneous Analysis" %in% out) {
				cmd01 <- paste(mod, "\n", cmdt0, "\n","summary(modMMR)")
				}
				if ("Hierarchical Analysis" %in% out) {
				cmd01 <- paste(mod, "\n", cmdt0, "\n",  "summary(modMMR, type= \"nested\")")
				}
				
				if ((length(pred) + length(mode1)) > 0) {
				cmd <- paste(cmd0, "\n", cmd01, "\n", cmd02, 
					"\n", cmdt2, "\n", "summary(Ssl)", "\n", 
					cmd07)
				}
				else {
				cmd <- paste(cmd0, "\n", cmd01, "\n", cmd07)
				}
			}
			else {
				
				
				
				if ("Simultaneous Analysis" %in% out) {
				if ((length(pred) + length(mode1)) > 0) {
					cmd01 <- paste(mod, "\n", cmdt0, "\n", "summary(modMMR)", "\n", cmd02, "\n", 
					cmdt2, "\n", "summary(Ssl)")
				}
				else {
					cmd01 <- paste(mod, "\n", cmdt0, "\n",  "summary(modMMR)")
				}
				}
				if ("Hierarchical Analysis" %in% out) {
				if ((length(pred) + length(mode1)) > 0) {
					cmd01 <- paste(mod, "\n", cmdt0, "\n",  "summary(modMMR, type= \"nested\")", 
					"\n", cmd02, "\n", cmdt2, "\n", "summary(Ssl)")
				}
				else {
					cmd01 <- paste(mod, "\n", cmdt0, "\n", "summary(modMMR, type= \"nested\")")
				}
				}
				cmd <- paste(cmd01, "\n", cmd07)
			}
		}
		else {
			if ("Mean Centered" %in% met) {
				cmd0 <- paste(modtemp)
				
				if ("Simultaneous Analysis" %in% out) {
				cmd01 <- paste(mod, "\n", cmdt0, "\n",  "summary(modMMR)")
				}
				if ("Hierarchical Analysis" %in% out) {
				cmd01 <- paste(mod, "\n", cmdt0, "\n",  "summary(modMMR, type= \"nested\")")
				}
				if ((length(pred) + length(mode1)) > 0) {
				cmd <- paste(cmd0, "\n", cmd01, "\n", cmd02, 
					"\n", cmdt2, "\n", "summary(Ssl)")
				}
				else {
				cmd <- paste(cmd0, "\n", cmd01)
				}
			}
			else {
				if ("Simultaneous Analysis" %in% out) {
				if ((length(pred) + length(mode1)) > 0) {
					cmd01 <- paste(mod, "\n", cmdt0, "\n",  "summary(modMMR)", "\n", cmd02, "\n", 
					cmdt2, "\n", "summary(Ssl)")
				}
				else {
					cmd01 <- paste(mod, "\n", cmdt0, "\n",  "summary(modMMR)")
				}
				}
				if ("Hierarchical Analysis" %in% out) {
				if ((length(pred) + length(mode1)) > 0) {
					cmd01 <- paste(mod, "\n", cmdt0, "\n",  "summary(modMMR, type= \"nested\")", 
					"\n", cmd02, "\n", cmdt2, "\n", "summary(Ssl)")
				}
				else {
					cmd01 <- paste(mod, "\n", cmdt0, "\n",  "summary(modMMR, type= \"nested\")")
				}
				}
				cmd <- paste(cmd01)
			}
			
		}
		
		execute(cmd)
		
	}
	
	
	
	dialog$setRunFunction(toJava(.MmrRunFunction))
	dialog
}

.getNewMmrDialog <- function() {
	if (!exists(".NewMmrDialog")) {
		ex <- globalenv()
		#make MMR dialog
		.NewMmrDialog <- .makeNewMmrDialog()
		assign(".NewMmrDialog", .NewMmrDialog, ex)
	}
	return(.NewMmrDialog)
}

.First.lib <- function(libname, pkgname) {
	
	
	.jpackage(pkgname, lib.loc = libname)
	
	

	deducer.addMenuItem("Moderated Regression", , ".getNewMmrDialog()$run()", 
		"Analysis")
	if (.windowsGUI) {
	
		winMenuAddItem("Analysis", "Moderated Regression", "deducer('Moderated Regression')")
	}
	else if (.jgr) {

		
		jgr.addMenuItem("Analysis", "Moderated Regression", "deducer('Moderated Regression')")
		
	}
	
}
