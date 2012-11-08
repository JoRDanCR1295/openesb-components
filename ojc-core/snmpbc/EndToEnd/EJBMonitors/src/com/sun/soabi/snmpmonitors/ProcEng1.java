/*
 * ProcEng1.java
 *
 * Created on April 3, 2007, 12:18 PM
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package com.sun.soabi.snmpmonitors;

/**
 *
 * @author fkieviet
 */
public class ProcEng1 {
    
    /** Creates a new instance of ProcEng1 */
    public ProcEng1() {
    }
    
    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {
        // TODO code application logic here
        System.out.println(getStats().getInvokes() + "/" + getStats().getItems());
    }
    
    public static com.sun.soabi.snmpmonitor1.Stats getStats() {
        com.sun.soabi.snmpmonitor1.StatsSvcService service = new com.sun.soabi.snmpmonitor1.StatsSvcService();
        com.sun.soabi.snmpmonitor1.StatsSvc port = service.getStatsSvcPort();
        com.sun.soabi.snmpmonitor1.Stats result = port.getStats();
        return result;
    }
}
