/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.sun.esb.management.client.tester;

import com.sun.jbi.engine.iep.management.api.IEPManagementService;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.StringTokenizer;

import java.util.Arrays;

/**
 *
 * @author rdwivedi
 */
public class DebugSession {
    public static final String ADMINISTRATION_KEY = "Administration";
    private String mPlanName = null;
    private IEPManagementService mSvc;
    
    private String mHelpString =
            "help,h: Prints this.\n" +
            "\n" +
            "quit,q: Quit.\n" +
            "\n" +
            "next,n : execute next available operator.\n"+
            "\n" +
            "completeCycle,cc : complete one cycle of IEP process.\n"+
            "\n" +
            "nextCompleteCycles,ncc {noOfTime(int)} : complete cycles of IEP process noOfTime times.\n"+
            "\n"+
            "nextCompleteCyclesForTimePeriod,nct {milliseconds (long)} : complete the cycles of IEP process for milliseconds time.\n"+
            "\n"+
            "addPauseBefore,apb {operatorName} : add a pause (breakpoint) before an IEP process.\n"+
            "\n"+
            "nextTillPause,ntp : execute IEP process till a pause is found or cycle is complete.\n"+
            "\n"+
            "nextOperator,no : display the name of the operator that is to be executed next.\n"+
            "\n"+
            "removePauseBefore,rpb {operatorName} : remove the pause before the operator if there is one available.\n"+
            "\n"+
            "listOperators,lo: List the operator names in sequence of execution. " +
            " It appends a '**' in front of operator names that are yet to be executed.\n"+
            "\n"+
            "listOperatorProperties,lop {operatorName} : List all the properties associated with an operator. " +
            "If operatorName is not provided the default will be used as last executed operator.\n" +
            "\n"+
            "info,i : show the relevant information about the last executed operator.\n"+
            "\n"+
            "info,i {oprName} : show the debug information about the oprName operator.\n\n";


    
    public DebugSession(String planName,IEPManagementService mgmtSvc) {
        mPlanName = planName;
        mSvc = mgmtSvc;
    }

    public String startSession() throws Exception {
        String result = setIEPForDebug(mPlanName);
        if(!result.equalsIgnoreCase("SUCCESS")){
            return result;
        }
        System.out.println("------------IEP Debug Console [  " + mPlanName + " ]----------");
        System.out.println("\n");
        System.out.println("\n");
        System.out.println("\n");
        System.out.println("\n");
        String cmd = null;
        while (cmd == null || !(cmd.equalsIgnoreCase("quit")|| cmd.equalsIgnoreCase("q"))) {
            
            try {
                cmd = getCommand();
                String[] pCmd = parseCommand(cmd);
                
                if(pCmd.length<1){
               //do nothing
                } else if ((pCmd[0].equalsIgnoreCase("help")||pCmd[0].equalsIgnoreCase("h"))) {
                    System.out.println("\n");
                    System.out.println("------------The list of debug commands ----------");
                    System.out.println(mHelpString);
                    System.out.println("-------------------------------------------"+"\n");
                } else   if (pCmd[0].equalsIgnoreCase("info")||pCmd[0].equalsIgnoreCase("i")){
                    String oprName = null;
                    if(pCmd.length < 2) {
                        // No operator name provided use default = latest executed operator.
                    } else {
                        oprName = pCmd[1];
                    }
                    String info = getCurrentDebugInfo(mPlanName,oprName);
                    System.out.println("\n");
                    System.out.println("\t DebugInfo\n\n" + info+"\n\n");
                } else if (pCmd[0].equalsIgnoreCase("quit")||cmd.equalsIgnoreCase("q")){
                    String result1 = unsetIEPForDebug(mPlanName);
                    System.out.println("" + result1+"\n");
                } else {
                    String[] mainArgs = null;
                    if(pCmd.length > 1) {
                        mainArgs = Arrays.copyOfRange(pCmd, 1, pCmd.length+1);
                    }
                    String result1 = callDebug(mPlanName,pCmd[0],mainArgs);
                    if(result1==null || result1.equalsIgnoreCase("null")){
                        System.out.println("\n");
                        System.out.println("Unknown command "+pCmd[0]+" . Use command 'help' to get a list of commands."+"\n");
                    } else {
                        System.out.println("\n");
                        System.out.println(result1+" \n");
                    }
                }
            } catch (Exception e) {
                e.printStackTrace();
                System.out.println("Error::" + e.getMessage());
            }
            System.out.println("\n");
        }
        return null;
    }
    public String getCurrentDebugInfo(String planName,String oprName){
        String targetName = null;
        String result = null;
        try {
            result = (String) mSvc.getCurrentDebugInfo(planName,oprName, targetName);
        } catch (Exception e) {
            // The various ways of error display
            e.printStackTrace();
        }
        return result;
    }
     
    private String setIEPForDebug(String planName) throws Exception {
        String targetName = null;
        return mSvc.setIEPForDebugging(planName, targetName);
        

    }
    private String  unsetIEPForDebug(String planName) throws Exception {
        String targetName = null;
        return mSvc.unsetIEPForDebugging(planName, targetName);
        
        
    }
    private String callDebug(String planName,String cmd,String args[]) throws Exception {

        String targetName = null;
        String result = null;
        result = (String)mSvc.executeDebugCommand(planName,cmd,args,targetName);
        
        return result;

        
    }

    
    public String getCommand() {
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));

        String command = null;
        System.out.print("[IEP][Debug " + mPlanName+"] ");
        try {
            command = br.readLine();
        } catch (IOException ioe) {
            System.out.println("IO error.");
            System.exit(1);
        }
        return command;
    }

    private String[] parseCommand(String cmd) {
        StringTokenizer st = new StringTokenizer(cmd);
        String[] str = new String[st.countTokens()];
        int i = 0;
        while (st.hasMoreTokens()) {
            String p = st.nextToken();
            //System.out.println(p);
            str[i++] = p;
        }
        return str;
    }
    
    
}
