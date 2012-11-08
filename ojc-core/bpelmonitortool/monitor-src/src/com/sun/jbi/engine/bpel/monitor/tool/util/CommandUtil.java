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
 * @(#)$Id: CommandUtil.java,v 1.6 2008/05/07 19:12:09 mei_wu Exp $
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.monitor.tool.util;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.MalformedURLException;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.ResourceBundle;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.management.MBeanServerConnection;
import javax.management.remote.JMXConnector;
import javax.management.remote.JMXConnectorFactory;
import javax.management.remote.JMXServiceURL;

import com.sun.caps.management.api.bpel.BPELManagementService;
import com.sun.caps.management.common.ManagementRemoteException;
import com.sun.em.common.GenericConstants;
import com.sun.em.connectors.GlassFishRemoteServerConnector;
import com.sun.jbi.engine.bpel.monitor.tool.Command;
import com.sun.jbi.engine.bpel.monitor.tool.CommandContext;
import com.sun.jbi.engine.bpel.monitor.tool.ValidationException;


public class CommandUtil {
	
	private static Pattern p1 = Pattern.compile("(.*?)=\"(.*?)\"");
	private static Pattern p2 = Pattern.compile(">\\s*\"(.*?)\"(\\+?)");
	
	public static class CommandData {
		public String name;
		public  Map<String, String> params = new HashMap<String, String> ();
		@Override
		public String toString() {
			// TODO Auto-generated method stub
			StringBuffer buffer = new StringBuffer ();
			buffer.append("name:");
			buffer.append(name);
			buffer.append("\n");
			
			for (Map.Entry<String, String> entry : params.entrySet()) {
				buffer.append(entry.getKey());
				buffer.append(":");
				buffer.append(entry.getValue());
				buffer.append("\n");
			}
			return buffer.toString();
		}		
		
	}
	
	public static CommandData parseCommandString (String str) {
		String [] subStrs = str.split("\\s");
		CommandData cmdData= new CommandData ();
		cmdData.params. put(Command.OUTPUT_FILE_APPEND, "false");		
		cmdData.name = subStrs[0];
		if (subStrs.length > 1) {
			//Find the quotes first
			String paramStr = str.substring(subStrs[0].length());
			Matcher matcher = p1.matcher(paramStr);
			while (matcher.find()) {
				cmdData.params. put(matcher.group(1).trim(),matcher.group(2).trim()); 				
			}			
			Matcher matcher2 = p2.matcher(paramStr);
			while (matcher2.find()) {
				cmdData.params. put (Command.OUTPUT_FILE, matcher2.group(1).trim());
				if (matcher2.groupCount() > 1 && matcher2.group(2).trim().equals("+")) {
						cmdData.params. put(Command.OUTPUT_FILE_APPEND, "true");						
				} 
			}					
		}
		return cmdData;
	}
	
	public static String getMessage (String defaultStr, Object... params) {
//		String stringFormat = null;
//		try {
//			bundle.getString(keyString);
//		}catch (Exception e) {
//			
//		}
//		if (stringFormat == null) {
//			stringFormat =  defaultStr;
//		}
//		return MessageFormat.format(stringFormat, params);
        
        return  I18n.loc(defaultStr, params);
	}

	public static void main(String[] args) {
		
		String cmd = "execute name=\"haze\" id=\"133434\" > \"a.xfz\"+";
		
		CommandUtil.parseCommandString(cmd);
		
		 cmd = "a";
		 CommandUtil.parseCommandString(cmd);
	}
	
	public static String getContextDisplay (String name) {
		InputStream in = Command.class.getResourceAsStream("messages/" + name + ".context");
		String fileStr  = null;
		try {
			 fileStr = readFile(in);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return fileStr;
	}
	
	public static String getCommandDisplay (String name) {
		InputStream in = Command.class.getResourceAsStream("messages/" + name + ".command");
		String fileStr  = null;
		try {
			 fileStr = readFile(in);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return fileStr;
	}	
	
    public static String readFile(String filename) throws FileNotFoundException, IOException {
        BufferedReader in = null;

        try {
            in = new BufferedReader(new InputStreamReader(new FileInputStream(filename), "UTF-8"));

        } catch (java.io.FileNotFoundException e) {
            throw e;

        }

        StringBuffer sb = new StringBuffer();

        // read into string
        try {
            String s = null;

            while ((s = in.readLine()) != null) {
                sb.append(s);
              }

        } catch (java.io.IOException e) {
            throw e;
        } finally {
            if (in != null)
                in.close();
        }

        return sb.toString();
    }
	
    public static String readFile(InputStream fileStream) throws IOException {
        BufferedReader in = null;

        in = new BufferedReader(new InputStreamReader(fileStream));
        StringBuffer sb = new StringBuffer();

        // read into string
        try {
            String s = null;

            while ((s = in.readLine()) != null) {
                sb.append(s);
                sb.append("\n");
            }

        } catch (java.io.IOException e) {
            throw e;
        } finally {
            if (in != null)
                in.close();
        }

        return sb.toString();
    }
    
    public static void displayPrompt () {
    	System.out.print(CommandContext.COMMAND_PROMPT);
    }
    
    public static String readComand () {
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));

        StringBuffer commandString = new StringBuffer ();
        String endChar = null;
 
        try {
        	while (endChar == null || endChar.equals("\\"))  {
        		String line = br.readLine().trim();
        		if (line.length() > 0) {
        			endChar = line.substring(line.length()-1);
        			if (endChar.equals("\\")) {
        				if (line.length() > 1) {
        					line = line.substring(0, line.length()-1);
        				} else {
        					line = "";
        				}        				
        			}
        			commandString.append(line);
        		} else {
        			endChar = "";
        		}
        	}
        } catch (IOException ioe) {
           System.out.println("IO error trying to read command!");
           System.exit(1);
        }

    	return commandString.toString();
    }
	    
    /**
     * Test if a string is empty.
     *
     * @param s String to test
     *
     * @return <code>true</code> if string is empty
     */
    public static boolean isEmpty(String s) {
        return ((null == s) || (s.trim().length() == 0));
    }    
    
	public static MBeanServerConnection getMbeanServerConnection(String hostName,
			int portNumber, String userName, String password) throws Exception {
		MBeanServerConnection connection = null;
		try {
	        GlassFishRemoteServerConnector remoteConnector = new GlassFishRemoteServerConnector(GenericConstants.SJSAS_SERVER_TYPE,
	                 "server1",hostName, portNumber+"", "8080", userName, password);
	        connection = remoteConnector.getConnection();
//			connection = RMIConnectionFactory.getInstance(hostName, portNumber, userName, password);
			return connection;
		} catch (Exception e) {
			throw e;
		} 
	}

	/**
	 * Singleton factory that gets an MBeanServerConnection using the RMI/JRMP
	 * connector
	 *  * @author graj
	 */
	public static class RMIConnectionFactory {
	        /** private constructor */
	    private RMIConnectionFactory() {
	    }
	        /**
	     * This method returns the MBeanServerConnection used to invoke attributes
	     * and operations on MBeans registered on GlassFish's MBean Server through
	     * the GlassFish supplied RMI/JRMP connector.
	     *      * @param hostName
	     * @param portNumber
	     * @param userName
	     * @param password
	     * @return
	     * @throws MalformedURLException
	     * @throws IOException
	     */
	    public static MBeanServerConnection getInstance(String hostName,
	            int portNumber, String userName, String password)
	            throws MalformedURLException, IOException {
	        MBeanServerConnection connection = null;
	        String urlString = "service:jmx:rmi:///jndi/rmi://" + hostName + ":"
	                + portNumber + "/jmxrmi";
	        return getMBeanServerConnection(urlString, userName, password);
	    }
	        /**
	     * This method returns the MBeanServerConnection used to invoke attributes
	     * and operations on MBeans registered on GlassFish's MBean Server through
	     * the GlassFish supplied RMI/JRMP connector.
	     *      * @param url -
	     *            service:jmx:rmi:///jndi/rmi://<hostName>:<portNumber>/management/rmi-jmx-connector
	     * @userName - the userName name for authenticating with MBeanServer
	     * @password - the password for authenticating with MBeanServer
	     * @return MBeanServerConnection
	     * @throws ManagementRemoteException
	     */
	    public static MBeanServerConnection getMBeanServerConnection(
	            String urlString, String userName, String password)
	            throws MalformedURLException, IOException {
	        final JMXServiceURL url = new JMXServiceURL(urlString);
	        String[] credentials = new String[] { userName, password };
	        Map<String, String[]> environment = new HashMap<String, String[]>();
	        environment.put("jmx.remote.credentials", credentials);
	        final JMXConnector connector = JMXConnectorFactory.connect(url,
	                environment);
	        return connector.getMBeanServerConnection();
	    } 	
	}

    public static List<String> parseCSVForInstanceIds(File file) throws Exception {
        List<String> instanceList = new ArrayList<String> ();
        BufferedReader in = null;

        try {
            in = new BufferedReader(new InputStreamReader(new FileInputStream(file), "UTF-8"));
        } catch (java.io.FileNotFoundException e) {
            throw e;
        }
        try {
            String s = null;
            boolean header = true;
            while ((s = in.readLine()) != null) {
                if (header) {
                    header = false;
                    continue;
                }
                String [] subStrs = s.split("\\,");
                instanceList.add(subStrs [0]);
            }
        } catch (java.io.IOException e) {
            throw e;
        } finally {
            if (in != null)
                in.close();
        }        
        return instanceList;
    }

    public static void checkMonitoringVariableEnabled(BPELManagementService service, String targetName) throws ValidationException {
        boolean isVariableMonitoringEnabled = false;
        try {
             isVariableMonitoringEnabled = service.isMonitoringVariableEnabled(targetName);
        }catch (Exception e) {
            e.printStackTrace();
            throw new ValidationException (I18n.loc("BPCOR-6022: Monitoring Variable is not enabled on BPEL service engine"));
        }
        if (!isVariableMonitoringEnabled) {
            throw new ValidationException (I18n.loc("BPCOR-6022: Monitoring Variable is not enabled on BPEL service engine"));
        }
    }
}
