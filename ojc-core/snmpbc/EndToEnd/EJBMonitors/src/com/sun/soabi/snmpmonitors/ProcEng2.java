/*
 * ProcEng2.java
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
public class ProcEng2 {
    
    /** Creates a new instance of ProcEng1 */
    public ProcEng2() {
    }
    
    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {
        // TODO code application logic here
        System.out.println(getStats().getInvokes() + "/" + getStats().getItems());
    }
    
    public static com.sun.soabi.snmpmonitor2.Stats getStats() {
        com.sun.soabi.snmpmonitor2.StatsSvc2Service service = new com.sun.soabi.snmpmonitor2.StatsSvc2Service();
        com.sun.soabi.snmpmonitor2.StatsSvc2 port = service.getStatsSvc2Port();
        com.sun.soabi.snmpmonitor2.Stats result = port.getStats();
        return result;
    }
}
