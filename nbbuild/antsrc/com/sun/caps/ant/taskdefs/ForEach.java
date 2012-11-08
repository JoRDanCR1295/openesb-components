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
 * @(#)ForEach.java
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

/**
 * For each specified location call ant task with specified target name.
 * If target name is not specified than name of current target is used.
 *
 * @author  Libor Kramolis
 */
public class ForEach extends Ant {
    private static final boolean DEBUG = true;
    private static final boolean ECHO  = true;

    private List   locations; // List<String>
    private List   targetList; // List<String>
    private String startdir;
    private boolean skipNonExistentDir = false;

    //
    // init
    //

    public ForEach () {
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

        Iterator it = locations.iterator();
        Runtime rt = Runtime.getRuntime();
        while ( it.hasNext() ) {
            if ( ECHO ) log ("totalMemory:" + rt.totalMemory() + ", freeMemory: " + rt.freeMemory());
            String dirName = (String) it.next();

            if ( ECHO ) log ("Process '" + dirName + "' location with '" + target + "' target ...");

            File dir = new File (baseDir, dirName);
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
            if ( DEBUG ) log ("--> next [ " + target + " ] " + dir.getAbsolutePath());

            try {
                if (target != null) {
                  ant.setTarget(target);
                  ant.execute();
                } else if (targetList != null) { // try target list.
                    Iterator itt = targetList.iterator();
                    while ( itt.hasNext() ) {
                        String targetName = (String) itt.next();
                        try {
                        ant.setTarget(targetName);
                        ant.execute();
                        } catch (Exception ex2) {
                            if ( DEBUG ) log (
                              "---------------------------------------\nTest Case Failed [ "
                              + targetName + " ] " + dir.getAbsolutePath() + "\n" + ex2.toString() +
                              "\n---------------------------------------\n");
                        }
                    }
                }
            } catch (Exception ex) {
                if ( DEBUG ) log (
                  "***************************************\nTest Project Failed [ "
                  + target + " ] " + dir.getAbsolutePath() + "\n" + ex.toString() +
                  "\n***************************************\n");
            }
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
