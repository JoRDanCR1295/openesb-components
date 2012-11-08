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
 * @(#)SetProjectPrivate.java
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.caps.ant.taskdefs;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.BufferedReader;
import java.io.FileReader;
import java.util.*;

import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.taskdefs.Ant;

import org.apache.tools.ant.Project;
import org.apache.tools.ant.Target;
import org.apache.tools.ant.Task;

/**
 * Set extra project private properties.
 *
 * @author  tli
 */
public class SetProjectPrivate extends Ant {
    private static final boolean DEBUG = false;
    private static final boolean ECHO  = true;
    private static final String nbprojct = "nbproject";
    private static final String userprop = "user.properties.file";
    private static final String buildprop = "/build.properties";

    private List     locations; // List<String>
    private String   rootDir;
    private boolean  reset = false;
    private boolean  clean = false;
    private String[] caps_build_properties = { "caps.alaska.root",
                                               "caps.netbeans.home",
                                               "caps.netbeans.user",
                                               "caps.appserver.home",
                                               "caps.jbi.home",
                                               "j2ee.platform.classpath",
                                               "j2ee.platform.wsimport.classpath",
                                               "j2ee.platform.wsgen.classpath"
    };

    public SetProjectPrivate () {
        // locations = new Vector();
    }

    /** remove all private properties
     */
    public void setClean (String s) {
        if ( DEBUG ) log ("clean  = " + s);

        clean = s.equalsIgnoreCase("true");
    }

    /** reset all private properties
     */
    public void setReset (String s) {
        if ( DEBUG ) log ("reset  = " + s);

        reset = s.equalsIgnoreCase("true");
    }

    /** set root directory
     */
    public void setRootDir (String s) {
        if ( DEBUG ) log ("root dir  = " + s);

        rootDir = s;
    }

    /** Netbeans project locations. */
    public void setLocations (String s) {
        if ( DEBUG ) log ("SET locations = " + s);

        StringTokenizer tok = new StringTokenizer (s, ",:");
        locations = new Vector ();
        while ( tok.hasMoreTokens() ) {
            locations.add (tok.nextToken().trim());
        }
    }

    private void addSystemProperty(String prop, Properties ps) {
        String pv = getProject().getProperty(prop);
        if (pv != null) {
           ps.setProperty(prop, pv);
        }
    }

    private void addUserProperty(String prop, Properties ps) {
        String pv = ps.getProperty(prop);
        String upv = getProject().getProperty(caps_build_properties[2]) + buildprop;
        ps.setProperty(prop, upv);
    }

    private boolean deleteDir(File dir) {
        if (dir.isDirectory()) {
            String[] children = dir.list();
            for (int i=0; i<children.length; i++) {
                boolean success = deleteDir(new File(dir, children[i]));
                if (!success) {
                    return false;
                }
            }
        }

        return dir.delete();
    }

    private void checkProjectDirs(File dir, List list, int prefix) {
        if (dir.isDirectory()) {
            if (dir.getName().equalsIgnoreCase(nbprojct)) {
                String fn = dir.getParentFile().getAbsolutePath();
              if ( DEBUG ) log("...addingDir: "+fn.substring(prefix, fn.length()));
                list.add(fn.substring(prefix, fn.length()));
            }
            String[] children = dir.list();
            for (int i=0; i<children.length; i++) {
                checkProjectDirs(new File(dir, children[i]), list, prefix);
            }
        }
    }

    private List getProjectLocations(String root) {
        List locs = new Vector ();
        if (root == null) return locs;

        File baseDir = getProject().getBaseDir().getParentFile();
        String pref = baseDir.getAbsolutePath();
        checkProjectDirs(new File(baseDir, root), locs, pref.length()+1);

        return locs;
    }

    /** Execute this task. */
    public void execute () throws BuildException {
        log ("set-project-private: start!");

        if (locations == null) {
            locations = getProjectLocations(rootDir);
        }

        if (locations.isEmpty()) {
            if ( DEBUG ) log ("SetProjectPrivate: No locations no loops!");
        }

        try {
           File baseDir = getProject().getBaseDir().getParentFile();

           Iterator it = locations.iterator();
           while ( it.hasNext() ) {
               String dirName = (String) it.next();
               File dir = new File (baseDir, dirName);
             log("processing: "+dir.getAbsolutePath());
               if (! dir.exists()) {
                   if ( DEBUG ) log ("Skipped non-existent module: " + dir.getAbsolutePath());
               } else {
                   File outputfile = new File(dir, "nbproject/private");
                   if (clean) {
                   if (outputfile.exists()) {
                          boolean del = deleteDir(outputfile);
                       }
                   } else {
                   if (! outputfile.exists()) {
                      outputfile.mkdirs();
                   }
                       writeProjectPrivateFile(new File(outputfile, "private.properties"));
                   }
               }
           }

        } catch (IOException e) {
        e.printStackTrace();
            this.log(e.getMessage());
        }

    }

    private void writeProjectPrivateFile(File file) throws IOException {
        InputStream in = null;
        OutputStream out = null;
        try {
            if (!file.exists()) {
                log("create file: "+file);
                file.createNewFile();
            }
            Properties p = new Properties();
            in = new BufferedInputStream(new FileInputStream(file));
            if (!reset) {
                p.load(in);
            }

            for (int i=0; i<caps_build_properties.length; i++) {
               addSystemProperty(caps_build_properties[i], p);
            }

            addUserProperty(userprop, p);

            out = new BufferedOutputStream(new FileOutputStream(file));
            p.store(out,"");
            if ( DEBUG ) log("writing to "+file.getAbsolutePath());
            if ( DEBUG ) log("content is: "+p);
        } finally {
            if (in != null) in.close();
            if (out != null) out.close();
        }
    }
}
