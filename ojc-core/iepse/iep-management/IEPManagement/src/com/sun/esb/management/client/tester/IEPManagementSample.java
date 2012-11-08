/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.esb.management.client.tester;

import com.sun.jbi.engine.iep.management.api.IEPManagementService;
import com.sun.jbi.engine.iep.management.service.IEPManagementServiceFactory;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.StringTokenizer;



import java.net.MalformedURLException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import javax.management.MBeanServerConnection;
import javax.management.remote.JMXConnector;
import javax.management.remote.JMXConnectorFactory;
import javax.management.remote.JMXServiceURL;



/**
 *
 * @author rdwivedi
 */
public class IEPManagementSample {

    private String mHostName = "localhost" ;
    private int mPortNumber = 8686;
    private String mUserName = "admin";
    private String mPwd = "adminadmin"; 
    // defaults  "localhost",8686,"admin","adminadmin"
    private String mHelpString =
            "h, help: Prints this.\n" +
            "\n" +
            "c,connect {host port userid password}: MBean connection parameters.\n" +
            "\n" +
            "q, quit: Quit.\n" +
            "\n" +
            "d, debug {plan_name}: Debug an IEP process.\n" +
            "\n" +
            "li, listIEP {service_unit_name}: List IEP process deployed in a Service unit" +
            " If Service unit name is not provided all the IEP process will be listed.\n" +
            "\n" +
            "lo, listOperators {plan_name}: List operators in an IEP process.\n" + 
            "\n" +
            "la, listAttributes {plan_name operator_name}: List visible properties of an Operator.\n" + 
            "\n" +
            "scs, startChangeSet : informs the IEP engine that a new change set request has been initiated.\n"+
            "\n"+
            "acs, applyChangeSet : informs the IEP engine to schedule all the changes done by using changePropValue,cpv to be applied and persisted. After this for any further changes startChangeSet,scs must be called first.\n"+
            "\n"+    
            "ics, ignoreChangeSet : informs the IEP engine to ignore the change set. After this for any further changes startChangeSet,scs must be called first.\n"+
            "\n"+
            "cpv, changePropValue {plan_name operator_name attribute_key attribute_value}: Sets the value of a property.\n" +
          //  "\n" +
          //  "eda, enableDA {plan_name operator_name table_name}: Enable Data Access for an operator into a table.\n" +
            "\n" +
            "addss ,addSaveStream {plan_name operator_name jndi_name tableName isGlobal<Optional>}: Add a save stream to an input stream operator.\n" +
            "\n" +
            "rss ,removeSaveStream {plan_name save_operator_name}: Removes the save stream operator.\n" +
            "\n" +
            "ep ,exportPlan {plan_name export_to_file, plan_version<Optional>}: Writes content of the specified plan_name and plan_version to a file. If plan_version is not specified then by default, the latest version of the plan is used.\n" +
            "\n" +
            "ppv ,purgePlanVersions {plan_name}: Deletes all versions of the specified plan_name from the version table.\n" +
            "\n" +
            "papv ,purgeAllPlanVersions {}: Deletes all versions of all the plans from the version table.\n";



    private IEPManagementService mMS = null;
    public IEPManagementSample() {
       
    }
    





    public static void main(String[] args) {
         IEPManagementSample tool = new IEPManagementSample();
         
         tool.startConsole();
    }
    
    private boolean validateConnection() {
        if(mMS==null) {
            System.out.println("Not Connected. Use 'connect' command to connect to " +
                    "MBean Server.");
            return false;
        } else {
            return true;
        }
    }
    public void startConsole(){
        
        System.out.println("------------IEPManagement Console----------");
        try {
            getIEPManagementService();
        } catch(Exception e) {
            System.out.println("Default connection failed. "+ "\n\n" +
                    " Use 'connect' command" +
                    " to establish a connection.");
        }
        
        String cmd = null;
        while(cmd==null || !(cmd.equalsIgnoreCase("quit")||cmd.equalsIgnoreCase("q"))){
           try {
            
           cmd =  getCommand();
           String[] pCmd = parseCommand(cmd);
           if(pCmd.length<1){
               //do nothing
           } else if ((pCmd[0].equalsIgnoreCase("help")||pCmd[0].equalsIgnoreCase("h"))) {
               System.out.println("------------The list of commands ----------");
               System.out.println(mHelpString);
               System.out.println("-------------------------------------------");


           }  else if ((pCmd[0].equalsIgnoreCase("startChangeSet")||pCmd[0].equalsIgnoreCase("scs"))) {
               startChangeSet();
           }   else if ((pCmd[0].equalsIgnoreCase("applyChangeSet")||pCmd[0].equalsIgnoreCase("acs"))) {
               applyChangeSet();
           }  else if ((pCmd[0].equalsIgnoreCase("ignoreChangeSet")||pCmd[0].equalsIgnoreCase("ics"))) {
               ignoreChangeSet();
           } else if ((pCmd[0].equalsIgnoreCase("connect")||pCmd[0].equalsIgnoreCase("c"))) {
               if(pCmd.length < 5) {
                   System.out.println("use 'connect hostName portNo userID password'.");
               } else {
                   setConnection(pCmd[1],Integer.parseInt(pCmd[2]),pCmd[3],pCmd[4]);
                   getIEPManagementService();
                   System.out.println("Connection successful.");
               }


           } else if (pCmd[0].equalsIgnoreCase("listIEP")||pCmd[0].equalsIgnoreCase("li")) {
               if (pCmd.length < 2) {
                   //Assuming service unit name is not provided list all deployed IEP;s
                   String[] list = listDeployedIEP(null);
                   System.out.println("------------The list of deployed IEPs----------");
                   if (list != null) {
                       for (int i = 0; i < list.length; i++) {
                           System.out.println(list[i]);
                       }
                   } else {
                       System.out.println("List is null. ");
                   }
                   System.out.println("-----------------------------------------------");
               } else {
                   String[] list = listDeployedIEP(pCmd[1]);
                   System.out.println("------------The list of deployed IEPs----------");
                   if (list != null && list.length>0) {
                       for (int i = 0; i < list.length; i++) {
                           System.out.println(list[i]);
                       }
                   } else {
                       System.out.println("List is null.\n");
                   }
                   System.out.println("-----------------------------------------------");
               }
           } else if (pCmd[0].equalsIgnoreCase("listOperators")||pCmd[0].equalsIgnoreCase("lo")) {
               if(pCmd.length < 2) {
                   System.out.println("listOperators command needs IEP plan name" +
                           " as parameter. To get plan name use listIEP command.");
               } else {
               String[] list = listOperatorsForIEP(pCmd[1]);
               System.out.println("------------The list of Operators for IEP [ "+ pCmd[1] + " ]--");
               if (list != null && list.length>0) {
               for(int i = 0 ; i < list.length;i++){
                 System.out.println(list[i]);
               } 
               } else {
                       System.out.println("List is null. Check the IEP Process Name\n");
                }
               System.out.println("-----------------------------------------------");
               }
           } else if (pCmd[0].equalsIgnoreCase("listAttributes")||pCmd[0].equalsIgnoreCase("la")) {
               if(pCmd.length < 3) {
                   System.out.println("listAttributes command needs IEP plan name,operator name  " +
                           " as parameters.");
               } else {
               Map list = getManagedAttributeForOperator(pCmd[1],pCmd[2]);
               System.out.println("------------The list of Atributes for IEP [ "+ pCmd[1] + ","+ pCmd[2]+" ]--");
               if (list != null) {
               Iterator iter = list.keySet().iterator();
               while(iter.hasNext()){
                   String k = (String)iter.next();
                   System.out.println("["+k+"="+ list.get(k)+"]");
               }
               } else {
                   System.out.println("List is null. Check the IEP Process Name and Operator Name.\n");
               }
               System.out.println("-----------------------------------------------");
               }
           } else if (pCmd[0].equalsIgnoreCase("changePropValue")||pCmd[0].equalsIgnoreCase("cpv")) {
               if(pCmd.length < 5) {
                   System.out.println("changePropValue command needs IEP plan name,operator name,attribute name" +
                           " and attribute value  " +
                           " as parameters.");
               } else {
               setManagedAttributeForOperator(pCmd[1],pCmd[2],pCmd[3],pCmd[4]);
               }
            } else if (pCmd[0].equalsIgnoreCase("enableDA")||pCmd[0].equalsIgnoreCase("eda")) {
               if(pCmd.length < 4) {
                   System.out.println("enableDA command needs IEP plan name,operator name,table name" +
                           "  " +
                           " as parameters.");
               } else { 
               String r = setEnableDataAccessObject(pCmd[1],pCmd[2],pCmd[3]);
               System.out.println("The result is " + r);
               }
            } else if (pCmd[0].equalsIgnoreCase("addSaveStream")||pCmd[0].equalsIgnoreCase("addss")) {
               if(pCmd.length < 5) {
                   System.out.println("addSaveStream command needs IEP plan name,operator name,jndi url" +
                           ",table name and isGlobal    " +
                           " as parameters. isGlobal is optional.");
               } else { 
               if(pCmd.length==5) {
                   //isGlobal is optional. default is true; 
                   String r = enableSaveStream(pCmd[1],pCmd[2],pCmd[3],pCmd[4],"true");
                    System.out.println("The result is " + r);
               } else { 
                    String r = enableSaveStream(pCmd[1],pCmd[2],pCmd[3],pCmd[4],pCmd[5]);
                    System.out.println("The result is " + r);
               }
               }
            } else if (pCmd[0].equalsIgnoreCase("removeSaveStream")||pCmd[0].equalsIgnoreCase("rss")) {
               if(pCmd.length < 3) {
                   System.out.println("removeSaveStream command needs IEP plan name,operator name,"+
                           " as parameters.");
               } else {  
               String r = removeSaveStream(pCmd[1],pCmd[2]);
               System.out.println("The result : " + r);
               }
            } else if (pCmd[0].equalsIgnoreCase("exportPlan")||pCmd[0].equalsIgnoreCase("ep")) {
                if(pCmd.length < 3) {
                    System.out.println("exportPlan command needs IEP plan name,file path where exported plan content will be written and plan version (optional),"+
                            " as parameters.");
                } else {  
                String planVersion = pCmd.length > 3 ? pCmd[3] : null;    
                String r = exportPlan(pCmd[1],pCmd[2],planVersion);
                System.out.println("The result : " + r);
                }
             } else if (pCmd[0].equalsIgnoreCase("purgePlanVersions")||pCmd[0].equalsIgnoreCase("ppv")) {
                 if(pCmd.length < 2) {
                     System.out.println("purgePlanVersions command needs IEP plan name"+
                             " as parameters.");
                 } else {  
                 String r = purgePlanVersions(pCmd[1]);
                 System.out.println("The result : " + r);
                 }
              } else if (pCmd[0].equalsIgnoreCase("purgeAllPlanVersions")||pCmd[0].equalsIgnoreCase("papv")) {
                  String r = purgeAllPlanVersions();
                  System.out.println("The result : " + r);
               } else if (pCmd[0].equalsIgnoreCase("debug")||pCmd[0].equalsIgnoreCase("d")){
               if(pCmd.length < 2) {
                   System.out.println("debug command needs IEP plan name"+
                           " as parameter.");
               } else {  
               DebugSession session = new DebugSession(pCmd[1],getIEPManagementService());
               String result = session.startSession();
               if(result!=null) {
                   System.out.println(result);
               }
               }
           } else if (pCmd[0].equalsIgnoreCase("quit")||pCmd[0].equalsIgnoreCase("q")){
                    // do nothing.
           } else {
               System.out.println("Unknown command. Use command 'help' to get a list of commands.");
           }
           } catch(IOException e){
                // do nothing;
           } catch( Exception e ) {
                e.printStackTrace();
                System.out.println("Error." + e.getMessage());
            }
        }
    }
    
    public String getCommand(){
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        
        String command = null;
        System.out.println(" \n");
        System.out.print("[IEP] ");
        try {
            command = br.readLine();
      } catch (IOException ioe) {
         System.out.println("IO error.");
         System.exit(1);
      }
        return command;
    }
    
    
    
    private String[] parseCommand(String cmd) {
        int length = 0;
        String[] preCmd = cmd.split("\"");
        if(preCmd.length == 2) {
            length = 1;
        }
        StringTokenizer st = new StringTokenizer(preCmd[0]);
        String[] str = new String[st.countTokens()+length];
        int i = 0;
        while (st.hasMoreTokens()) {
            String p = st.nextToken();
            str[i++] = p;
        }
        if(length == 1){
          str[i] =  preCmd[1]; 
        }
        return str;
    }
    
    

    private IEPManagementService getIEPManagementService() throws Exception {
        
        if(mMS == null) {
            try {
                mMS = IEPManagementServiceFactory.createNewServiceInstance
                        (getConnection());
            } catch(Exception ex){
                
                System.out.println("Service not available. Please use 'connect' " +
                        "command to establish connection.");
                throw ex;
           }
        }
        return mMS;
    }
    private String startChangeSet() throws Exception {
        String targetName = null;
        String result = null;
        result = (String) getIEPManagementService().startNewChangeSet(targetName);
        return result;

    }
    private String applyChangeSet() throws Exception {
        String targetName = null;
        String result = null;
        result = (String) getIEPManagementService().applyChangeSet(targetName);
        return result;

    }
    
    private String ignoreChangeSet() throws Exception {
        String targetName = null;
        String result = null;
        result = (String) getIEPManagementService().ignoreChangeSet(targetName);
        return result;

    }
    
    
    private MBeanServerConnection getConnection() throws Exception {
        return RMIConnectionFactory.getInstance(mHostName,mPortNumber,mUserName,mPwd);
    }


    private String setEnableDataAccessObject(String planName, String operatorName, String tabName) throws Exception {

        String targetName = null;
        String result = null;
        try {
            result = (String)getIEPManagementService().setEnableDataAccessObject(planName,operatorName,tabName, targetName);
        } catch (Exception e) {
            // The various ways of error display
            //e.printStackTrace();
            throw e;
        }
        return result;

    }
    private String disableDataAccess(String planName, String operatorName) throws Exception {

        String targetName = null;
        String result = null;
        try {
            result = (String)getIEPManagementService().disableDataAccess(planName,operatorName, targetName);
        } catch (Exception e) {
            // The various ways of error display
            //e.printStackTrace();
            throw e;
        }
        return result;

    }

    private String enableSaveStream(String planName, String operatorName , String jndi, String tableName,String isGlobal) throws Exception {
        String targetName = null;
        String result = null;
        try {
            result = (String)getIEPManagementService().enableSaveStream(planName,operatorName,jndi,tableName,isGlobal, targetName);
        } catch (Exception e) {
            // The various ways of error display
            //e.printStackTrace();
            throw e;
        }
        return result;

    }

    private String removeSaveStream(String planName, String saveStreamName) throws Exception {
        String targetName = null;
        String result = null;
        try {
            result = (String)getIEPManagementService().removeSaveStream(planName,saveStreamName, targetName);
        } catch (Exception e) {
            // The various ways of error display
            //e.printStackTrace();
            throw e;
        }
        return result;

    }


    private String[] listDeployedIEP(String suName) throws Exception {
        //String targetName = "server";
        String targetName = null;
        String[] result = null;
        try {
            result = (String[])getIEPManagementService().listDeployedIEP(suName, targetName);
        } catch (Exception e) {
            // The various ways of error display
            //e.printStackTrace();
            throw e;
        }
        return result;
        
    }
    private void setManagedAttributeForOperator(String planName, String oprName,String propName,Object value) throws Exception{
        //String targetName = "server";
        String targetName = null;
        String result = null;
        try {
            getIEPManagementService().setManagedAttributeForOperator(planName,oprName,propName,value,targetName);
        } catch (Exception e) {
            // The various ways of error display
            //e.printStackTrace();
            throw e;
        }
        //return result.booleanValue();
    }
    private String[] listOperatorsForIEP(String planName) throws Exception {
        String targetName = null;
        String[] result = null;
        try {
            result = (String[])getIEPManagementService().listOperatorsForIEP(planName, targetName);
        } catch (Exception e) {
            // The various ways of error display
            //e.printStackTrace();
            throw e;
        }
        return result;

    }

    private String exportPlan(String planName, String exportToFilePath, String planVersion) throws Exception {
        //String targetName = "server";
        String targetName = null;
        String result = null;
        try {
            File exportToFile = new File(exportToFilePath);
            result = getIEPManagementService().exportPlan(planName, planVersion, targetName);
            if(result != null) {
                //write the result to file
                FileWriter fw = new FileWriter(exportToFile);
                fw.write(result);
                fw.close();
                
                result = "Plan "+ planName + " successfully exported to "+ exportToFile.getAbsolutePath();
            } else {
                result = "No version information is found for Plan "+ planName + " in plan version table.";
            }
        } catch (Exception e) {
            // The various ways of error display
            //e.printStackTrace();
            throw e;
        }
        return result;
        
    }
    
    private String purgePlanVersions(String planName) throws Exception {
        //String targetName = "server";
        String targetName = null;
        String result = null;
        try {
            result = getIEPManagementService().purgePlanVersions(planName,  targetName);
        } catch (Exception e) {
            // The various ways of error display
            //e.printStackTrace();
            throw e;
        }
        return result;
        
    }
    
    private String purgeAllPlanVersions() throws Exception {
        //String targetName = "server";
        String targetName = null;
        String result = null;
        try {
            result = getIEPManagementService().purgeAllPlanVersions(targetName);
        } catch (Exception e) {
            // The various ways of error display
            //e.printStackTrace();
            throw e;
        }
        return result;
        
    }
    
    private Map getManagedAttributeForOperator(String planName,String oprName) 
            throws Exception {
        String targetName = null;
        Map result = null;
        try {
            result = (Map)getIEPManagementService().getManagedAttributeForOperator(planName, oprName, targetName);
        } catch (Exception e) {
            // The various ways of error display
            //e.printStackTrace();
            throw e;
        }
        return result;

    }
    
    private void setConnection(String host,int port,String user,String pwd){
        mHostName = host;
        mPortNumber = port;
        mUserName = user;
        mPwd = pwd;
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
	     * @throws Exception
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
}
