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
 * @(#)ForEachDriverTest.java
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.caps.ant.taskdefs;

import java.io.File;
import java.util.*;

import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.taskdefs.Ant;
import org.apache.tools.ant.taskdefs.Java;
import org.apache.tools.ant.taskdefs.Property;
import org.apache.tools.ant.taskdefs.Recorder;
import org.apache.tools.ant.taskdefs.Recorder.ActionChoices;
import org.apache.tools.ant.types.Path;
import org.apache.tools.ant.types.Commandline.Argument;
import org.apache.tools.ant.types.Path.PathElement;


/**
 * For each specified location call ant task with specified target name.
 * If target name is not specified than name of current target is used.
 *
 * @author  Libor Kramolis
 * @author  Sanjay Sharma
 */
public class ForEachDriverTest extends Ant {
    private static final boolean DEBUG = true;
    private static final boolean ECHO  = true;

    private List   locations; // List<String>
    private List   targetList; // List<String>
    private String startdir;
    private String logdir;
    private String prefix;
    private boolean skipNonExistentDir = false;


    public ForEachDriverTest () {
        locations = new Vector();
    }

    /** Comma separated list of locations. */
    public void setLocations (String s) {
        if ( DEBUG ) log ("SET locations = " + s);

        StringTokenizer tok = new StringTokenizer (s, ",:");
        locations = new Vector ();
        while ( tok.hasMoreTokens() ) {
            locations.add (tok.nextToken().trim());
        }
    }

    public void setTargetList (String s) {
        if ( DEBUG ) log ("SET targetList = " + s);

        StringTokenizer tok = new StringTokenizer (s, ",:");
        targetList = new Vector ();
        while ( tok.hasMoreTokens() ) {
            targetList.add (tok.nextToken().trim());
        }
    }

    //override setDir() to noop
    public void setDir() {}

    private String target;
    public String getTarget() {
        return target;
    }

    public void setTarget(String v) {
        target = v;
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

        if ((target == null) && (targetList == null)) {
            setTarget(this.getOwningTarget().getName());

            if ( DEBUG ) log ("EXECUTE owningTarget = " + this.getOwningTarget());
        }
        File baseDir;
        if ( startdir == null ) {
            baseDir = getProject().getBaseDir();
        } else {
            baseDir = new File(getProject().getBaseDir(), startdir);
        }

		ActionChoices start = new ActionChoices();
		start.setValue("start");

		ActionChoices stop = new ActionChoices();
		stop.setValue("stop");

		Recorder recorder = (Recorder)getProject().createTask("record");

        Iterator it = locations.iterator();
        while ( it.hasNext() ) {

            String dirName = (String) it.next();

            //String dirNameStr1 = dirName.substring(5);
            String dirNameStr = dirName.replace('/','.');

			//java.util.Hashtable properties = (Hashtable)getProject().getProperties();
			//log(properties.toString());
			//log(dirNameStr1);
			//log(dirNameStr2);

			//String rootDir = System.getenv("BUILD_OUTPUT_HOME");

			recorder.setName(logdir + File.separator + prefix + "." + dirNameStr + ".properties");
			recorder.setAction(start);
			recorder.execute();

            if ( ECHO ) log ("Process '" + dirName + "' location with '" + target + "' target ...");

            File dir = new File (baseDir, dirName);

            // create the '\src\jbiasa' directory if it does not exists
            File jbiscaDir = new File(baseDir, dirName + File.separator + "src" + File.separator + "jbiasa");
            if ( !jbiscaDir.exists() ) {
            	boolean status = false;

            	try {
                    status = jbiscaDir.mkdir();
            	} catch ( Exception e ) {
            	    log ("Failed to create : " + jbiscaDir.getAbsolutePath() + " : " + e.getMessage());
            	}
            	if ( !status ) {
                    log ("Failed to create : " + jbiscaDir.getAbsolutePath());
                    return;
		}
	    }

            File buildXml = new File(dir, "build.xml");
            if (! buildXml.exists() && skipNonExistentDir) {
                if ( DEBUG ) log ("Skipped non-existent " + dir.getAbsolutePath());
                return;
            }

            String location = (new File(dir, "build.xml")).getAbsolutePath();
            Ant ant = (Ant) getProject().createTask("ant");
            ant.init();
            ant.setLocation(getLocation());
            ant.setAntfile(location);
            ant.setInheritAll(getInheritAll());

            String emmaEnableValue = "false";
            String emmaHomeValue = null;

            Enumeration e = properties.elements();
	    	log("set properties");

            while (e.hasMoreElements()) {

                Property t = (Property) e.nextElement();
                if ( "driver.emma.enabled".equals(t.getName()) ) {
                    emmaEnableValue = t.getValue();
                }

                if ( "emma.home".equals(t.getName()) ) {
                    emmaHomeValue = t.getValue();
                }

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
            if ( DEBUG ) log ("--> next [ " + target + " ] " + dir.getAbsolutePath());

            if ( "true".equals(emmaEnableValue) ) {
            	resetCoverageData(emmaHomeValue);
            }

            try {
                ant.setTarget(target);
                ant.execute();
            } catch (Exception ex) {
                if ( DEBUG ) log (
                  "***************************************\nTest Project Failed [ "
                  + target + " ] " + dir.getAbsolutePath() + "\n" + ex.toString() +
                  "\n***************************************\n");
            }
            recorder.setAction(stop);
            recorder.execute();

            if ( "true".equals(emmaEnableValue) ) {
            	getCoverageData(emmaHomeValue, dir);
            	resetCoverageData(emmaHomeValue);
            }

        }

    }

    private void getCoverageData(String emmaHome, File currDir) {
    try {
        Java java = (Java)getProject().createTask("java");
        java.setClassname("emma");

        Path path = java.createClasspath();

        PathElement pe1 = path.createPathElement();
        pe1.setLocation(new File(emmaHome + File.separator + "emma.jar"));

        Argument arg1 = java.createArg();
        arg1.setValue("ctl");

        Argument arg2 = java.createArg();
        arg2.setValue("-connect");

        Argument arg3 = java.createArg();
        arg3.setValue("localhost:47653");

        Argument arg4 = java.createArg();
        arg4.setValue("-command");

        Argument arg5 = java.createArg();
        arg5.setValue("coverage.get," + (new File(currDir,"driver_coverage.emma")).getAbsolutePath());

        java.execute();

    } catch ( Throwable th ) {
        if ( DEBUG ) log (
                "***************************************\nDumping coverage data... failed \n");
        }
    }

    private void resetCoverageData(String emmaHome ) {
        try {
            Java java = (Java)getProject().createTask("java");
            java.setClassname("emma");

            Path path = java.createClasspath();

            PathElement pe1 = path.createPathElement();
            pe1.setLocation(new File(emmaHome + File.separator + "emma.jar"));

            Argument arg1 = java.createArg();
            arg1.setValue("ctl");

            Argument arg2 = java.createArg();
            arg2.setValue("-connect");

            Argument arg3 = java.createArg();
            arg3.setValue("localhost:47653");

            Argument arg4 = java.createArg();
            arg4.setValue("-command");

            Argument arg5 = java.createArg();
            arg5.setValue("coverage.reset");

            java.execute();
        } catch ( Throwable th ) {
            if ( DEBUG ) log (
                    "***************************************\nresetting coverage data... failed \n");
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