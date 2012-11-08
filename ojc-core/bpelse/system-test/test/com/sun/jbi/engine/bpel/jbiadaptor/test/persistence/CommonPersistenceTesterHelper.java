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
 * @(#)CommonPersistenceTesterHelper.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.jbiadaptor.test.persistence;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URI;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.StringTokenizer;

import junit.framework.TestCase;

import com.sun.jbi.engine.bpel.core.bpel.util.BPELHelper;
import com.sun.jbi.engine.bpel.jbiadaptor.test.common.SetUpHelper;
import com.sun.jbi.engine.bpel.jbiadaptor.test.common.TestDBConnImpl;


/**
 *
 * @author Sun Microsystems
 */
public class CommonPersistenceTesterHelper {
    public static final String PARTNER_MYROLE = "myRole";
    public static final String PARTNER_PARTNERROLE = "partnerRole";

    private static final String CRASHPOINTS = "CRASHPOINTS";
    private static final String RECOVERY = "RECOVERY";
    private static final String RECOVERY_VAL = "In Development";

    static String mClassPath = "";

    static boolean setUp() throws Exception {
        new SetUpHelper();
        mClassPath = System.getProperty("junit.suite.classpath");
        return false;    // false indicates tests using Wait (vs. Invoke) will NOT be executed
    }
    
    protected static void commonCode(String folder, String propertiesFilename) throws Exception {
        URL url = CommonPersistenceTesterHelper.class.getResource(folder);
        String deployedFolderPath = folder + "/deployedFolder";
        File testinputdir = new File(url.toURI().getPath() + File.separator + "input");

        File testfile = new File(testinputdir, propertiesFilename);
        if (!testfile.exists()) {
            TestCase.fail("Cannot execute test, file doesn't exist: " + propertiesFilename);
        }

        URI propFilePath = testfile.toURI();
        String crmpId = BPELHelper.getGUID();
        int status = execCommand(propFilePath, deployedFolderPath, EngineDriver.NO_CRASH_RECOVERY_ID, 
        		EngineDriver.TestMode.PERSIST, crmpId);        
        TestCase.assertTrue(status == EngineDriver.SUCCESSFUL_EXECUTION);
        
        cleanup(propFilePath);
        
        crashTest(propFilePath, deployedFolderPath);
    }

    private static void cleanup(URI propFilePath) {
        File propFile = new File(propFilePath);
        File parentDir = propFile.getParentFile().getParentFile();
        File tmpDir = new File(parentDir.getPath() + File.separator + "temp");

        if (tmpDir.isDirectory() && tmpDir.exists()) {
            File[] children = tmpDir.listFiles();

            for (int i = 0; i < children.length; i++) {
                children[i].delete();
            }

            tmpDir.delete();
        }
    }

    private static int execCommand(URI propFilePath, String deployedFolderPath, int crashpoint, 
    		EngineDriver.TestMode testMode, String crmpId)
        throws Exception {
        String classpath = mClassPath;

        Properties props = new Properties();
        props.load(propFilePath.toURL().openStream());
        String skipPersistenceRun = props.getProperty("NO_PERSISTENCE_RUN");
        if (skipPersistenceRun != null && skipPersistenceRun.equalsIgnoreCase("yes")
                && testMode == EngineDriver.TestMode.PERSIST) {
          return 0;
        }
        int debugPort = -1;
        // if prop is missing, will default to false
        boolean debug = Boolean.valueOf(props.getProperty("DEBUG")).booleanValue();
        boolean crashOnly = Boolean.valueOf(props.getProperty("SUSPEND_CRASH_ONLY")).booleanValue();
        if (testMode != EngineDriver.TestMode.RECOVER && crashOnly) {
            debug = false;  // only suspend after crash
        }
        if (debug) {
            debugPort = Integer.parseInt(props.getProperty("DEBUG_PORT"));
        }

        String debugopts = "";
        if (debugPort > 0) {
            System.out.println("debugging on port: "+ debugPort);
            debugopts = String.format("-Xdebug -Xrunjdwp:transport=dt_socket,server=y,suspend=y,address=%s", debugPort);
        }
        //adding a min mem size of 256m and option to dump a .hprof file on OutOfMemoryError exception
        //debugopts += " -XX:+HeapDumpOnOutOfMemoryError";

        String cmmd = String.format(
        "java %s -classpath %s com.sun.jbi.engine.bpel.jbiadaptor.test.persistence.EngineDriver %s %s %s %s %s",
            debugopts,
            classpath,
            propFilePath.getRawPath(), 
            deployedFolderPath,
            crashpoint,
            testMode.toString(),
            crmpId);

        Process p = Runtime.getRuntime().exec(cmmd);
        String input = "This is Line 1\nThis is Line 2\n";
        ByteArrayInputStream in = new ByteArrayInputStream(input.getBytes());

        // create threads to handle I/O streams
        (new IO("stdin", in, p.getOutputStream())).start();
        (new IO("stdout", p.getInputStream(), System.out)).start();
        (new IO("stderr", p.getErrorStream(), System.err)).start();

        // wait for process to exit
        int status = p.waitFor();
        System.err.println(status);

        return status;
    }

    private static void crashTest(URI propFilePath, String deployedFolderPath) 
    throws Exception {

        Properties prop = new Properties();
        prop.load(propFilePath.toURL().openStream());
        if (RECOVERY_VAL.equals(prop.getProperty(RECOVERY))) {
        	//This means recovery is not to be tested
            return;
        }

        List<Integer> crashPointVals = getCrashPoints(propFilePath, prop);

        for (int i = 0; i < crashPointVals.size(); i++) {
            int crashPointVal = ((Integer) crashPointVals.get(i)).intValue();
            System.out.println(">>>>>>>>>>>>>>>>>>>>>>> Crashing at CRASHPOINT " + crashPointVal + " >>>>>>>>>>>>>>>>>>>>>>>");
            // crmp id across crash and recovery should be same 
            String crmpId = BPELHelper.getGUID();

            int status = execCommand(propFilePath, deployedFolderPath, crashPointVal, 
            		EngineDriver.TestMode.CRASH, crmpId) & 0x7F;
            int toCompare = (TestDBConnImpl.SUCCESSFUL_EXIT_ID) & 0x7F;
            System.out.println("out" + toCompare);
            TestCase.assertTrue(status == toCompare);

            System.out.println(">>>>>>>>>>>>>>>>>>>>>>> Recovering after crashing at CRASHPOINT " + crashPointVal + " >>>>>>>>>>>>>>>>>>>>>>>");
            status = execCommand(propFilePath, deployedFolderPath, crashPointVal, EngineDriver.TestMode.RECOVER, crmpId);
            TestCase.assertTrue(status == EngineDriver.SUCCESSFUL_EXECUTION);
            cleanup(propFilePath);
        }
    }

    private static List<Integer> getCrashPoints(URI propFilePath, Properties prop){

        File propFile = new File(propFilePath);
        File baseDir = propFile.getParentFile().getParentFile();
        String testoutputfile = baseDir.getPath() + File.separator + "output" 
        									+ File.separator + prop.getProperty("DBSTEPS");
        
        int count = getTotalNumberOfCrashPoints(testoutputfile);
        if(count < 1) {
            TestCase.fail("There should be atleast 1 CRASHPOINT. The number of CRASHPOINTS" +
            		" as read from the file " + testoutputfile + " is " + count);
        }
        
        System.out.println("Total number of possible CRASHPOINTS is " + count);
        
        String crashPoints = prop.getProperty(CRASHPOINTS);
        
    	List<Integer> crashPointVals = new ArrayList<Integer>();

    	if(crashPoints != null) {
    		StringTokenizer tokenizer = new StringTokenizer(crashPoints, ",");
    		while (tokenizer.hasMoreTokens()) {
    			Integer intVal = null;
    			String token = tokenizer.nextToken();
    			// allow for crash point ranges (i.e. {first}-{last}, inclusive)
    			int hyphen = token.indexOf('-');
    			if (hyphen > 0) {
    				Integer start = parseInt(token.substring(0, hyphen), count),
    				end = parseInt(token.substring(hyphen + 1), count);
    				if (start == null && end == null) {
    					System.out.println("Cannot parse CRASHPOINTS range: " + String.valueOf(start) + " - " +
    							String.valueOf(end));
    				}
    				else {
    					crashPointVals.add(start);
    					int first = start.intValue() + 1,
    					last = end.intValue() - 1;
    					for (int i = first; i <= last; i++) {
    						crashPointVals.add(Integer.valueOf(i));
    					}
    					crashPointVals.add(end);
    				}
    			} else {
    				intVal = parseInt(token, count);
    				if (intVal != null) crashPointVals.add(intVal);
    			}
    		}

    	}

    	if (crashPointVals.isEmpty()) {
    		System.out.println("No value for CRASHPOINTS specified or the value could not be parsed." +
    				" Recovery will be tested by crashing all possible CRASHPOINTS.");
    		for(int i=1; i<=count; i++) {
    			crashPointVals.add(Integer.valueOf(i));
    		}
    	} else {
    		for(Integer integer : crashPointVals) {
    			if(integer.intValue() < 1 || integer.intValue() > count) {
    	            TestCase.fail("Invalid value specified for CRASHPOINTS. Valid values for CRASHPOINTS" +
    	            		" are numbers from 1 to " + count);
    			}
    		}
    		
    		System.out.println("Using the specified CRASHPOINTS for testing recovery : " + crashPoints);
    	}

    	return crashPointVals;
    }

    private static int getTotalNumberOfCrashPoints (String file) {
    	int numCrashPoints = 0;
    	BufferedReader in = null;
    	try {
        	in = new BufferedReader(new FileReader(file));
        	String line = new String();
        	while((line = in.readLine())!= null)
        	{
        		if (line.startsWith("CRASHPOINT : ")) {
        			numCrashPoints++;
        		}
        	}        	
        	in.close();
    		
    	} catch (Exception e) {
    		if(in != null) {
    			try {
					in.close();
				} catch (IOException e1) {
					e1.printStackTrace();
				}
    		}
    		throw new RuntimeException(e);
    	}
    	
    	return (numCrashPoints - 1);
    }
    
    private static Integer parseInt(String token, int count) {
        Integer intVal = null;
        try {
            intVal = Integer.valueOf(token);
        }
        catch (NumberFormatException nfe) {
            System.out.println("Ignoring the CRASHPOINT, since the value is invalid; value = " + token);
        }

        return intVal;
    }

    static class IO extends Thread {
        private InputStream in;
        private OutputStream out;

        IO(String name, InputStream inStream, OutputStream outStream) {
            this.in = inStream;
            this.out = outStream;
            setName(name);
        }

        public void run() {
            try {
                int c;

                while ((c = in.read()) != -1) {
                    out.write(c);
                }

                out.flush();
            } catch (IOException e) {
            } finally {
                if (!System.out.equals(out) && !System.err.equals(out)) {
                    // Note: in order to get an exec'd java process to
                    // see EOF on input, it is necessary to close stdin
                    if (out != null) {
                        try {
                            out.close();
                        } catch (Exception e) {
                        }
                    }
                }
            }
        }
    }
}
