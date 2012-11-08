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
 * @(#)$Id: CommandDriver.java,v 1.7 2008/04/08 20:24:48 mei_wu Exp $
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.monitor.tool;

import java.io.IOException;
import java.io.InputStream;
import java.util.Map;
import java.util.Properties;
import java.util.ResourceBundle;

import javax.management.MBeanServerConnection;
import javax.management.ObjectName;

import com.sun.caps.management.api.bpel.BPELManagementService;
import com.sun.caps.management.api.bpel.BPELManagementServiceFactory;
import com.sun.caps.management.impl.bpel.BPELManagementServiceImpl;
import com.sun.esb.management.api.administration.AdministrationService;
import com.sun.esb.management.api.configuration.ConfigurationService;
import com.sun.esb.management.client.ManagementClient;
import com.sun.esb.management.client.ManagementClientFactory;
import com.sun.esb.management.common.ManagementRemoteException;
import com.sun.jbi.engine.bpel.monitor.tool.util.CommandUtil;
import com.sun.jbi.engine.bpel.monitor.tool.util.I18n;
import com.sun.jbi.engine.bpel.monitor.tool.util.CommandUtil.CommandData;

public class CommandDriver   {
	private static final String ERROR_IN_COMMAND = "BPCOR-6003: Error in Command : {0}";
	private static final String NULL_COMMAND = "BPCOR-6004: Can not find the specific command in current context : {0}";
	private static final String UNABLE_TO_GET_MBEANSERVERCONNECTION = "BPCOR-6005: Unable to get MbeanServerConnection -- host:{0}, port:{1}, user:{2}, password:{3}";
	
	private static final String JMX_HOST="JmxHostName";
	private static final String JMX_USER = "JmxUserName";
	private static final String JMX_PWD = "JmxPassword";
	private static final String JMX_PORT = "JmxPort";
    private static final String TARGET_NAME = "TargetName";
    private static final String BPEL_COMPONENT="sun-bpel-engine";
    private static final String ADMINISTRATION_KEY = "Administration";
    private static final String SERVER_TARGET="server";
	
	public static void main(String[] args) {
		String jmxUserName= null;
		String jmxPwd = null;
		int jmxPort = 0;
		if (args.length == 1) {
			jmxPwd = args [0].trim();
		} else if (args.length == 2) {
			jmxUserName = args [0].trim();
			jmxPwd = args [1].trim();
		} 
		InputStream connectionIn= CommandDriver.class.getResourceAsStream("/monitorcommand.properties");
		Properties connectionProp = new Properties ();
		try {
			connectionProp.load(connectionIn);
		} catch (IOException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
			System.exit(1);
		}
//		DummyTxManagerAndDataSource dataSource = new DummyTxManagerAndDataSource (connectionProp);		
		String jmxHostName = connectionProp.getProperty(JMX_HOST, "localhost");
		if (jmxPwd != null) {
			jmxUserName = connectionProp.getProperty(JMX_USER, "admin");
		} else {
			jmxUserName = connectionProp.getProperty(JMX_USER, "admin");
			jmxPwd= connectionProp.getProperty(JMX_PWD, "adminadmin");
		}
		jmxPort = Integer.parseInt(connectionProp.getProperty(JMX_PORT, "8686"));
//		MBeanServerConnection mbeanServerConn = null;
//		try {
//			mbeanServerConn = CommandUtil.getMbeanServerConnection(jmxHostName, jmxPort, jmxUserName, jmxPwd);
//		}catch (Exception e) {
//			String errorMsg = I18n.loc(UNABLE_TO_GET_MBEANSERVERCONNECTION, jmxHostName, jmxPort, jmxUserName, jmxPwd);
//			e.printStackTrace();
//			System.exit (1);
//		}
//		if (mbeanServerConn == null) {
//			String errorMsg = I18n.loc(UNABLE_TO_GET_MBEANSERVERCONNECTION, 
//					jmxHostName, jmxPort, jmxUserName, JMX_PWD);
//			System.exit (1);			
//		}
		String targetName = connectionProp.getProperty(TARGET_NAME);

		ContextResource cr = new ContextResource ();
//		cr.setDataSource(dataSource);
		BPELManagementService mbeanService = null;
        try  {
            mbeanService = BPELManagementServiceFactory.getBPELManagementServiceRemote(jmxHostName, jmxPort, jmxUserName, jmxPwd);
        }catch (Exception e) {
            e.printStackTrace();
            System.exit (1);
        }        
		cr.setMBpelService(mbeanService);
        cr.setTargetName(targetName);
		
        if (targetName == null) {
            checkNonCluster(jmxHostName, jmxPort, jmxUserName, jmxPwd, SERVER_TARGET);
        }
		CommandContext currentContext = new MainContext ("Main", null, cr);
		boolean displayContext = true;
		while (currentContext != null) {
			if (displayContext) {
				currentContext.display();
				CommandUtil.displayPrompt();
			} else {
				displayContext = true;
			}
			String commandStr = null;
			while (commandStr == null || commandStr.length() == 0) {
				commandStr = CommandUtil.readComand();
			}
			Command cmd = null;
			CommandData cmdData = null;
			String errorMsg = null;
			try {
				 cmdData = CommandUtil.parseCommandString(commandStr);
			} catch (Exception e) {
				errorMsg = I18n.loc(ERROR_IN_COMMAND, commandStr);			
			}
			CommandContext childContext = currentContext.lookupChildContext(cmdData);
			boolean valid = false;		
			if (childContext == null) {
				cmd = currentContext.lookupCommand(cmdData);
				if (cmd != null) {
					cmd.setParams(cmdData.params);
					try {
						cmd.validate();
						valid = true;
					} catch (ValidationException e) {
						// TODO Auto-generated catch block
						errorMsg = e.getMessage();
						valid = false;
					}catch (Exception e) {
						// TODO Auto-generated catch block
						e.printStackTrace(System.out);
						valid = false;
					}
				}
				if (cmd == null) {
					if (errorMsg == null) {
						errorMsg = I18n.loc(NULL_COMMAND, commandStr);
					}
					System.out.println(errorMsg);
				} else if (!valid) {
					System.out.println(errorMsg);
					cmd.display();			
					CommandUtil.displayPrompt();
					displayContext = false;
					cmd = null;
				} else {
					try {
                        if (targetName != null) {
                            checkCluster(jmxHostName, jmxPort, jmxUserName, jmxPwd, targetName);
                        }
						long before = System.currentTimeMillis();
						currentContext = cmd.execute();
						long after = System.currentTimeMillis();
						System.out.println("Command executed :" + (new Long(after-before)).toString()+ " ms");
						if (currentContext != null) {
							CommandUtil.displayPrompt();
							displayContext = false;
						}
					} catch (Exception e) {
						// TODO Auto-generated catch block
						e.printStackTrace(System.out);
					}
				}
			} else {
				currentContext = childContext;
			}
			
		}
		
		 System.out.println("GoodBye!");
		
	}

    private static boolean  invokeExtensionMBeanOperations(ConfigurationService configService, String componentName, String extensionName, String targetName, String targetInstanceName) {
        Boolean result = Boolean.FALSE;
        
        Object[] params = null;
        String[] signatures = null;
        try {
            result = (Boolean)configService.invokeExtensionMBeanOperation(componentName, extensionName, 
                    "isMonitoringEnabled", params, signatures, targetName, targetInstanceName);
        } catch (ManagementRemoteException e) {
            // The various ways of error display
            System.out.println("Target: "+targetName+" is not up");
            System.out.println(e.getMessage());
            System.exit(1);   
        }
        return result.booleanValue();
    }
        

    private static void checkNonCluster(String hostName,
            int portNumber, String userName, String password, String targetName) {
        try {
            ManagementClient client = null;
            AdministrationService administrationService = null;
            ConfigurationService configurationService = null;
            client = ManagementClientFactory.getInstance(hostName, portNumber, userName, password);
            Map<String /*targetName*/, String[] /*instanceNames*/> targetNameToInstancesMap = null;
            try {
                administrationService = client.getAdministrationService();
                targetNameToInstancesMap = administrationService.listTargetNames();
                boolean isUp = administrationService.isTargetUp(targetName);
                if (!isUp) {
                    System.out.println(" Instance is not Up");
                    System.exit(1);   
                }
                configurationService = client.getConfigurationService();
                boolean isMonitorEnabled = invokeExtensionMBeanOperations(configurationService, BPEL_COMPONENT, ADMINISTRATION_KEY, targetName, null);
                    if (!isMonitorEnabled) {
                        System.out.println(" Monitoring is not Enabled.");
                        System.exit(1);   
                    }
               
            } catch (ManagementRemoteException e) {
                // The various ways of error display
                String[] causes = e.getCauseMessageTrace();
                int index = 0;
                System.out.println("Received Exception - Cause Message Trace:");
                for(String cause : causes) {
                    System.out.println("Cause #"+(++index)+": "+cause);
                }
                System.exit(1);            
            }            
        } catch (ManagementRemoteException e) {
            // The various ways of error display
            e.printStackTrace();
            System.exit(1);
        }
    }
    
    private static void checkCluster(String hostName,
            int portNumber, String userName, String password, String targetName) {
        try {
            ManagementClient client = null;
            AdministrationService administrationService = null;
            ConfigurationService configurationService = null;
            client = ManagementClientFactory.getInstance(hostName, portNumber, userName, password);
            Map<String /*targetName*/, String[] /*instanceNames*/> targetNameToInstancesMap = null;
            try {
                administrationService = client.getAdministrationService();
                targetNameToInstancesMap = administrationService.listTargetNames();
                String [] values = null;
                if(targetNameToInstancesMap != null) {
                    values =  targetNameToInstancesMap.get(targetName);
                    if (values == null) {
                        System.out.println("Target "+targetName+" is not a cluster. ");
                        System.exit(1);
                    } else if (!administrationService.isTargetUp(targetName)) {
                        System.out.println("Cluster "+targetName+" is not up. ");
                        System.exit(1);
                    }
                } else {
                    System.out.println("Target "+targetName+" is not a cluster. ");
                    System.exit(1);                    
                }
                configurationService = client.getConfigurationService();
                for(String value : values) {
                    boolean isMonitorEnabled = invokeExtensionMBeanOperations(configurationService, BPEL_COMPONENT, ADMINISTRATION_KEY, targetName, value);
                    if (!isMonitorEnabled) {
                        System.out.println("Cluster Target "+ targetName +":" + value + " Monitoring is not Enabled");
                        System.exit(1);   
                    }
                }                
            } catch (ManagementRemoteException e) {
                // The various ways of error display
                String[] causes = e.getCauseMessageTrace();
                int index = 0;
                System.out.println("Received Exception - Cause Message Trace:");
                for(String cause : causes) {
                    System.out.println("Cause #"+(++index)+": "+cause);
                }
                System.exit(1);            
            }            
        } catch (ManagementRemoteException e) {
            // The various ways of error display
            e.printStackTrace();
            System.exit(1);
        }
    }




}
