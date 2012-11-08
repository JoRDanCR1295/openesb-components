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
 * @(#)Cmd.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.toolkit.project.util;

import java.util.ArrayList;
import java.util.List;
import com.sun.jbi.common.util.Util;
import com.sun.jbi.component.toolkit.project.model.Project;

/**
 * 
 * @author Kevan Simpson
 */
public class Cmd implements Exec {
    private String mName;
    
    public Cmd(String name) {
        mName = name;
    }

    /** @see com.sun.jbi.component.toolkit.project.util.Exec#execute(com.sun.jbi.component.toolkit.project.model.Project, java.lang.String[]) */
    public String[] execute(Project proj, String... args) {
        if (proj != null && args != null) {
            try {
                // resolve tokens
                final String[] cmds = new String[args.length];
                for (int i = 0, n = args.length; i < n; i++) {
                    cmds[i] = resolveArg(args[i]);
                }
                
                final Process[] procArray = new Process[1];
                final List<String> output = new ArrayList<String>();
                final int[] codeArray = new int[] { -1 };
                Runnable run = new Runnable() {
                    public void run() {
                        try {
                            Process proc = Runtime.getRuntime().exec(cmds);
                            procArray[0] = proc;
                            StreamGobbler out = 
                                    new StreamGobbler(proc.getInputStream(), output);
                            StreamGobbler err = 
                                    new StreamGobbler(proc.getErrorStream(), output);
                            out.start();
                            err.start();
                            codeArray[0] = procArray[0].waitFor();
                        }
                        catch (Exception e2) {
                            System.out.println("CODE: "+ e2.getMessage());
                            e2.printStackTrace();
                        }
                    }
                };

                Thread t = new Thread(run, mName +" command thread");
                t.start();
                // on my machine, start-domain takes just over 20 seconds
                // start-domain is given 30sec, other commands 0
                t.join(30000);//Math.abs(AsAdmin.this.getWaitTime()));
                
                int code = codeArray[0];
                System.out.println("Exit code for "+ mName +"="+ code);

                if (procArray[0] == null) {
                    return new String[] { mName +" command failed to complete!" };
                }

                return (output.isEmpty()) 
                        ? new String[] { "Command "+ mName +" generated no output." } 
                        : output.toArray(new String[output.size()]);
            }
            catch (Exception e) {
                e.printStackTrace();
            }
        }            
        
        return new String[0];
    }

    /** @see java.lang.Object#toString() */
    @Override
    public String toString() {
        return mName;
    }

    protected String resolveArg(String str) {
        if (str.contains("${")) {
            int a = str.indexOf("${"), z = str.indexOf("}", a);
            String key = str.substring(a + 2, z), 
                   prop = System.getProperty(key);
            if (Util.isEmpty(prop)) {
                throw new ProjectException("Uninitialized System Property: "+ key); 
            }
            else {
                 return resolveArg(str.substring(0, a) + prop + str.substring(z + 1));
            }
        }

        return str;
    }
}
