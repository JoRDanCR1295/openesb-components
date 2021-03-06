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
 * @(#)Debugger.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.jbiadaptor.test.bpeldebugger;

import java.io.File;
import java.io.IOException;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.Map.Entry;

import org.netbeans.modules.bpel.debuggerbdi.rmi.api.BPELDebugger;
import org.netbeans.modules.bpel.debuggerbdi.rmi.api.BPELProcessInstanceRef;
import org.netbeans.modules.bpel.debuggerbdi.rmi.api.BPELProcessRef;
import org.netbeans.modules.bpel.debuggerbdi.rmi.api.BPELVariable;
import org.netbeans.modules.bpel.debuggerbdi.rmi.api.DebugFrame;
import org.netbeans.modules.bpel.debuggerbdi.rmi.api.DebugListener;
import org.netbeans.modules.bpel.debuggerbdi.rmi.api.DebuggableEngine;
import org.netbeans.modules.bpel.debuggerbdi.rmi.api.VirtualBPELEngine;
import org.netbeans.modules.bpel.debuggerbdi.rmi.wp.ObjectAdapter;
import org.netbeans.modules.bpel.debuggerbdi.rmi.wp.RMIClient;
import org.netbeans.modules.bpel.debuggerbdi.rmi.wp.RMIServer;
import org.netbeans.modules.bpel.debuggerbdi.rmi.wp.RMIService;
import org.netbeans.modules.bpel.debuggerbdi.rmi.wp.RMIServiceFactory;
import org.netbeans.modules.bpel.debuggerbdi.rmi.wp.impl.DefaultRMIServiceFactory;

import com.sun.jbi.engine.bpel.jbiadaptor.test.common.MasterTestProperties;
import com.sun.jbi.engine.bpel.jbiadaptor.test.common.TestProperties;

public class Debugger implements BPELDebugger {

	boolean attached = false;

	DebuggerEngine dbEngine;

	boolean hasGotOutput = false;
	
	AttachClient attachClient = null;
    
    VirtualBPELEngine virtualBpelEngine = null;

	class ClientDebuggerListernStub implements DebugListener {
		private BPELDebugger bpDebugger = null;

		public void setDebugger(BPELDebugger debugger) {
			// TODO Auto-generated method stub
			bpDebugger = debugger;
		}

		public void socketClosed(Object arg0) {
			// TODO Auto-generated method stub
			if (bpDebugger != null) {
				bpDebugger.detach();
				System.out.println("Debugger is detached");
			}

		}
	}

	class AttachClient {

		BPELDebugger debugger;

		DebugListener localListener;

		RMIService rmi;

		RMIServer server;

		ObjectAdapter oa;

		BPELDebugger bpelDebugger = null;

		String host = "localhost"; // TODO give some value

		String port = System.getProperty("debugPort", "3343"); //TODO give some value
		
		DebugListener listener = null;

		AttachClient(BPELDebugger debugger) throws Exception {
			this.debugger = debugger;
			localListener = new ClientDebuggerListernStub();
			rmi = getRMIService();
			server = rmi.createServer(0);
			oa = server.createObjectAdapter("root");
			server.setDefaultAdaptor(oa);
			oa.start();
			new Thread(server).start();
		}
		
		public void detach () {
			listener.setDebugger(null);	
		}

		public void attach () {
			bpelDebugger = null;
			int portNum = 0;

			try {
				portNum = Integer.parseInt(port);
			} catch (Exception e) {
			}

			try {
				RMIClient cli = rmi.createClient(host, portNum);
				cli.setObjectAdapter(oa);
				System.out.println("Trying to connect to = " + host + ":"
						+ port);
				listener = (DebugListener) cli
						.importObject(DebugListener.class, "root",
								"debugListener");
				oa.registerListerner(localListener);
				System.out.println("registered listener");

				try {
					Object obj = oa.exportObject(debugger);
					bpelDebugger = (BPELDebugger) obj;
					localListener.setDebugger(debugger);
				} catch (IOException exc) {
					exc.printStackTrace();
					System.out.println("Attach Error \n" + exc.getMessage());
				}

				new Thread() {
					String errMsg = null;

					public void run() {
						try {
							listener.setDebugger(bpelDebugger);
							System.out.println("Connected to: " + host + ":"
									+ port);
							attached = true;

						} catch (Exception exc) {
							exc.printStackTrace();
							final String errorMessage = "Unable to connect to "
									+ host + ":" + port;
							System.out
									.println("Attach Error \n" + errorMessage);
						}
					}
				}.start();

			} catch (Exception exc) {
				final String errorMessage = "Unable to connect to " + host
						+ ":" + port;
				System.exit(1);
			}
		}
	}

	class DebuggerFrame implements DebugFrame {

		int lineNumber;

		/** DOCUMENT ME! */
		String uri;

		/** DOCUMENT ME! */
		DebuggableEngine engine;
        
        String parentFrameId;
        
        String rootFrameId;
        
        String id;

		DebuggerFrame(String rootFrameId, String parentFrameId, String id) {
            this.rootFrameId = rootFrameId;
            this.parentFrameId = parentFrameId;
            this.id = id;            
		}

		public void onLineChange(String bpelFile, String uri,
				int lineNumber, String xPath, DebuggableEngine engine) {
            this.engine = engine;
			// TODO Auto-generated method stub
            System.out.println("*** Activity starts: " + id);
            System.out.println("*** bpelFile:" + bpelFile);
            System.out.println("*** xPath:" + xPath);
			System.out.println("*** lineNumber:" + lineNumber);
 
			if (isDebugDetail()) {
				inspectVariables ();
			}
            printExpression(lineNumber);
            
            if (bpelFile.equals("echo.bpel") && lineNumber == 63) {
                try {
                    System.out.println("%%%%%%Variable old value:" + engine.getContainerDataAsString("echo_Output"));
                    engine.changeVariableMessageTypeValue("echo_Output", "part", "/tns:value", "changedValue");
                    System.out.println("%%%%%%Variable new value:" + engine.getContainerDataAsString("echo_Output"));
                    
                    System.out.println("%%%%%%Variable old value:" + engine.getContainerDataAsString("echo_temp"));
                    engine.changeVariableSchemaTypeValue("echo_temp", "/tns:value", "changedValue2");
                    System.out.println("%%%%%%Variable new value:" + engine.getContainerDataAsString("echo_temp"));
                    
                } catch (Exception e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }
                
            }
            
            if (bpelFile.equals("echo.bpel") && lineNumber == 45) {
                try {
                    System.out.println("%%%%%%Variable initialized $echo_Output:" + engine.getVariable("echo_Output").isInitialized());
                    System.out.println("%%%%%%Variable initialized $echo_Input:" + engine.getVariable("echo_Input").isInitialized());
                    System.out.println("%%%%%%Variable initialized $echo_temp:" + engine.getVariable("echo_temp").isInitialized());
                } catch (Exception e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }
                
            }            
            
            if (bpelFile.equals("forEach_seq1.bpel") && lineNumber == 55) {
                try {
                    System.out.println("%%%%%%Variable old value:" + engine.getContainerDataAsString("forEachCounter1"));
                    engine.changeVariableSchemaTypeValue("forEachCounter1", "4");
                    System.out.println("%%%%%%Variable new value:" + engine.getContainerDataAsString("forEachCounter1"));
                    
                    System.out.println("%%%%%%Variable initialized:" + engine.getVariable("forEachCounter1").isInitialized());
                    System.out.println("%%%%%%Variable xsd data:" + engine.getVariable("forEachCounter1").getXSDData());
                    System.out.println("%%%%%%Evaluate:" + engine.evaluate("$forEachCounter1"));
                    
                } catch (Exception e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }
                
            }
            
            
		}

		private void printExpression(int lineNumber) {
            // TODO Auto-generated method stub
            MasterTestProperties masterTestProps = getMasterProps();
            Set testsEntrySet = masterTestProps.sends.entrySet();
            File baseFile = masterTestProps.baseDir;
            for (Iterator it = testsEntrySet.iterator(); it.hasNext();) {
                Entry testEntry = (Entry) it.next();    
                TestProperties testProperties = (TestProperties) testEntry.getValue();
                List testVarLineNos = testProperties.testVarLineNos;
                List testVarExprs = testProperties.testVarExpr;
                if (testVarLineNos != null && testVarLineNos.size() > 0) {
                    for (int i = 0; i<testVarLineNos.size(); i ++) {
                        int testline = Integer.parseInt(((String)testVarLineNos.get(i)));
                        if (testline == lineNumber) {
                            String exp = (String) testProperties.testVarExpr.get(i);
                            try {
                                String value = engine.evaluate(exp);
                                System.out.println("******Exp:" + exp);
                                System.out.println("******Var:" + value);
                            }catch (Exception e) {
                                System.out.println("******Failed to evaluate: " + exp + " line:" + lineNumber);
                                e.printStackTrace();
                            }
                            
                        }
                    }
                }
            }
                

        }

        private void inspectVariables(){
			// TODO Auto-generated method stub
			String [] containers = engine.getVariables();			
			if (containers == null || containers.length == 0) {
				System.out.println("NO containers");
				return;
			}
			for (int i=0; i< containers.length; i++ ) {
				System.out.println("Container name:" + containers[i]);
				System.out.println("Value:" + engine.getContainerDataAsString(containers[i]));
            }
	
		}



		public void onFault(String bpelFile, String uri, int lineNumber, String xPath) {
			// TODO Auto-generated method stub

		}

		public void onXPathException(String bpelFile, String uri,
				int lineNumber, String message, String xPath) {
			// TODO Auto-generated method stub

		}

		public void onTerminate(String bpelFile, String uri,
				int lineNumber, String xPath) {
			// TODO Auto-generated method stub

		}

		public void onExit(String bpelFile, String uri) {
			// TODO Auto-generated method stub
			System.out.println("<<< Frame" + id + " ends, bpelFile:" + bpelFile);

		}

        public String getId() {
            // TODO Auto-generated method stub
            return id;
        }

        public String getProcessInstanceId() {
            // TODO Auto-generated method stub
            return rootFrameId;
        }

        public void onFault(String bpelFile, String uri, int lineNumber, String xpath, String faultQName, BPELVariable faultData, DebuggableEngine engine) {
        }

        public String getParentFrameId() {
            // TODO Auto-generated method stub
            return parentFrameId;
        }

        public void onActivityComplete(String bpelFile, String uri, int lineNumber, String xpath) {
            // TODO Auto-generated method stub
            System.out.println("&&&& Activity completes: " + id);
            System.out.println("&&&&COMPLETED bpelFile:" + bpelFile);
            System.out.println("&&&&COMPLETED xPath:" + xpath);
            // TODO Auto-generated method stub
            System.out.println("&&&&COMPLETED lineNumber:" + lineNumber);


        }

        public void onSubActivityComplete(String bpelFile, String uri, int lineNumber, String xpath) {
        }

 
	}

	public Debugger() {

	}
	
	public static void main(String[] args) {
		
		Debugger debugger = new Debugger ();
		try {
			debugger.attach();
		} catch (Exception e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
		while (true) {
			try {
				System.out.println("wait...");
				Thread.sleep(10000000);
			} catch (InterruptedException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}

		
	}
	

	public void setUp() {
		startDebuggerEngine();
	}

	private void startDebuggerEngine() {
		dbEngine = new DebuggerEngine();
		new Thread(dbEngine).run();
        int i = 0;
		while (!dbEngine.started && i<20) {
			System.out.println("Debugger Engine not started yet, waiting...");
			try {
				Thread.sleep(1000);
                i++;
			} catch (InterruptedException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		} 
        if (!dbEngine.started) {
            throw new RuntimeException ("Debug Engine can not be started!");
        }
		System.out.println("Debugger Engine started.");

	}

	/**
	 * DOCUMENT ME!
	 *
	 * @param args DOCUMENT ME!
	 */
	public void attach() throws Exception {

		if (attachClient == null) {
			attachClient =  new AttachClient(this);
		}
		new Thread() {
            public void run() {
            	attachClient.attach();
            }
		}.start();
		
		int i = 0;
		while (!attached && i<20) {
			try {
				System.out.println("Not Attached yet, wait...");
				Thread.sleep(100);
				i++;
			} catch (InterruptedException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}

	public boolean isAttached() {
		return attached;
	}

	public boolean outputProduced() {
		return hasGotOutput;
	}

	public void sendMessage() throws Exception {
		// TODO Auto-generated method stub
//		attach();
		System.out.println("Sending the message");
		hasGotOutput = dbEngine.sendMessage();
//		while (!hasGotOutput) {
//			try {
//				System.out.println("No output yet, wait...");
//				Thread.sleep(100);
//			} catch (InterruptedException e) {
//				// TODO Auto-generated catch block
//				e.printStackTrace();
//			}
//		}

	}

//	private void sendMessage(DebuggerEngine dbEngine2) {
//		// TODO Auto-generated method stub
//		while (!attached) {
//			try {
//				System.out.println("Not Attached yet, wait...");
//				Thread.sleep(100);
//			} catch (InterruptedException e) {
//				// TODO Auto-generated catch block
//				e.printStackTrace();
//			}
//		}
//
//		System.out.println("Sending the message");
//		hasGotOutput = dbEngine.sendMessage();
//
//	}

	private static RMIService getRMIService() {
		try {
			RMIServiceFactory factory = (RMIServiceFactory) new DefaultRMIServiceFactory();

			return factory.createRMIService(Debugger.class.getClassLoader());
		} catch (Exception e) {
			e.printStackTrace();

			return null;
		}
	}

	public boolean detach()  {
		// TODO Auto-generated method stub
		if (attached)  {
			attached = false;
			hasGotOutput = false;
			attachClient.detach();			
		}
        return true;
	}

	public DebugFrame enterFrame(String id, String rootFrameId,  String parentFrameId, String bpelFile, String uri) {
		// TODO Auto-generated method stub
		System.out.println("<<<enter frame...<<<" + "id: " + id +  " rootFrameId: "+ rootFrameId + " parentFrameId: " + parentFrameId);
		return new DebuggerFrame(rootFrameId, parentFrameId, id);
	}
    
//    public DebugFrame enterFrame(String rootFrameId, String parentFrameId, String bpelFile, String uri) {
//        return null;
//    }
	
	
	public boolean isDebugDetail () {
		return dbEngine.isDebugDetail ();
	}

    public boolean processRemoved(String targetNamespace) {
        // TODO Auto-generated method stub
        return false;
    }
    
    public MasterTestProperties getMasterProps () {
        return dbEngine.getMasterProps();
    }

    public void processAdded(BPELProcessRef process) {
        // TODO Auto-generated method stub
        
    }

    public void processInstanceDied(BPELProcessInstanceRef instance) {
        // TODO Auto-generated method stub
        
    }

    public void processInstanceStarted(BPELProcessInstanceRef instance) {
        // TODO Auto-generated method stub
        
    }

    public void processRemoved(BPELProcessRef process) {
        // TODO Auto-generated method stub
        
    }

    public void setVirtualBPELEngine(VirtualBPELEngine engine) {
        // TODO Auto-generated method stub
        virtualBpelEngine = engine;
        String [] allBpels = virtualBpelEngine.allDeployedBPELs();
        for (int i=0; i < allBpels.length; i++) {
            BPELProcessRef ref =  virtualBpelEngine.getBPELProcess(allBpels [i]);
            System.out.println("^^^^Deployed Bpel:" + allBpels [i]);
            System.out.println("^^^^Process instances: " + ref.allProcessInstanceIDs().length);
        }
    }

}
