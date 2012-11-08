/*
 * BEGIN_HEADER - DO NOT EDIT
 *
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)TestForEachInstallable.java
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.caps.ant.taskdefs;

import java.io.File;
import java.util.*;

import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.taskdefs.Ant;
import org.apache.tools.ant.taskdefs.Property;
import org.apache.tools.ant.taskdefs.Recorder;
import org.apache.tools.ant.taskdefs.Recorder.ActionChoices;
 
/**
 * @author  Sanjay Sharma
 */
public class TestForEachInstallable extends Ant {
    private static final boolean DEBUG = true;
    private static final boolean ECHO  = true;
    private String logdir;
    private String prefix;

    private List   locations; 
    private String startdir;
    private boolean skipNonExistentDir = false;
    private String installtype;

    public TestForEachInstallable () {
        locations = new Vector();
    }

    public void setLocations (String s) {
        if ( DEBUG ) log ("SET locations = " + s);
        StringTokenizer tok = new StringTokenizer (s, ",:");
        locations = new Vector ();
        while ( tok.hasMoreTokens() ) {
            locations.add (tok.nextToken().trim());
        }
    }

    private boolean inheritAll;
    
    public boolean getInheritAll() {
        return inheritAll;
    }
    public void setInheritAll(boolean v) {
        inheritAll = v;
    }

    public void setPrefix (String s) {
        if ( DEBUG ) log ("SET Prefix = " + s);

        prefix = s;
    }
    
    public void setLogdir (String s) {
        if ( DEBUG ) log ("SET logdir = " + s);

        logdir = s;
    }    
    
    /** Where cd first
     */
    public void setStartdir (String s) {
        if ( DEBUG ) log ("SET startdir = " + s);

        startdir = s;
    }

    public void setSkipNonExistentDir(boolean v) {
        skipNonExistentDir = v;
    }
    public boolean getSkipNonExistentDir() {
        return skipNonExistentDir;
    }

    public void setInstallType(String installType) {
    	this.installtype = installType;
    }

    public String getInstallType() {
        log ("installed type : " + installtype);
    	return this.installtype;
    }

    /** the properties to pass to the new project */
    private Vector properties = new Vector();

    /**
     * Property to pass to the new project.
     * The property is passed as a 'user property'.
     * @return the created <code>Property</code> object.
     */
    public Property createProperty() {
        Property p = new Property();
        p.setProject(getProject());
        p.setTaskName("property");
        properties.addElement(p);
        return p;
    }

    /** Execute this task. */
    public void execute () throws BuildException {
        log ("execute");
        if (locations.isEmpty()) {
            if (skipNonExistentDir) {
                if ( DEBUG ) {
                    log ("for-each: No locations no loops!");
		}
                return;
            } else {
                throw new BuildException("You must set at least one location!", getLocation());
            }
        }

        File baseDir;
        baseDir = new File(getProject().getBaseDir(), startdir);
        
        ActionChoices start = new ActionChoices();
        start.setValue("start");

        ActionChoices stop = new ActionChoices();
        stop.setValue("stop");

        Recorder recorder = (Recorder)getProject().createTask("record");
        
        Iterator it = locations.iterator();
        while ( it.hasNext() ) {
        	
            String location  = (String) it.next();
        	
            String dirNameStr = null;

            if ( logdir != null && !logdir.trim().equals("") ) {
            	dirNameStr = location.replace('/','.');
            	recorder.setName(logdir + File.separator + prefix + "." + dirNameStr + ".properties");
		recorder.setAction(start);
		recorder.execute();
            }
        	        	
            if ( this.installtype.equalsIgnoreCase("uninstall")) {
                    executeTarget(baseDir, location, "jbi-stop");
                    executeTarget(baseDir, location, "jbi-uninstall");
            } else {
                    executeTarget(baseDir, location, "jbi-start");
            }
        	
            if ( logdir != null && !logdir.equals("") ) {
	            recorder.setAction(stop);
	            recorder.execute();
            }
        	
        }

    }

    public void executeTarget(File baseDir, String dirName, String target) {
        if ( ECHO ) log ("Process '" + dirName + "' location with '" + target + "' target ...");

        File dir = new File (baseDir, dirName);

        Ant ant = (Ant) getProject().createTask("ant");
        ant.init();
        ant.setDir(dir);
        
        ant.setAntfile("build.xml");
        ant.setInheritAll(getInheritAll());
        Enumeration e = properties.elements();
        log("set properties");

        while (e.hasMoreElements()) {
            Property t = (Property) e.nextElement();

            Property p = ant.createProperty();
            log("set ["+t.getName()+"] to ["+t.getValue()+"]");
            p.setName(t.getName());
            p.setValue(t.getValue());
            p.execute();
        }
        if (getPropertyName() != null && getPropertyValue() != null) {
            org.apache.tools.ant.taskdefs.Property p = ant.createProperty();
            p.setName(getPropertyName());
            p.setValue(getPropertyValue());
        }
        if ( DEBUG ) log ("--> next " + dir.getAbsolutePath());

        try {
            ant.setTarget(target);
            ant.execute();            
        } catch (Exception ex) {
            if ( DEBUG ) log (
              "***************************************\nTest Project Failed [ "
              + target + " ] " + dir.getAbsolutePath() + "\n" + ex.toString() +
              "\n***************************************\n");
        }

    }

    public void handleErrorOutput(String output) {
	log(output);
    }

        private String propertyName;
        private String propertyValue;

        public String getPropertyName() {
            return propertyName;
        }
        public void setPropertyName(String name) {
            propertyName = name;
        }
        public String getPropertyValue() {
            return propertyValue;
        }
        public void setPropertyValue(String v) {
            propertyValue = v;
        }

}
