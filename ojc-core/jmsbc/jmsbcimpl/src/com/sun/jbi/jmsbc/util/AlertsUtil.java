package com.sun.jbi.jmsbc.util;

import com.sun.jbi.alerter.Alerter;
import com.sun.jbi.alerter.AlerterImpl;

/**
 *
 * @author Sun
 */
final public class AlertsUtil {
    public static final String SERVER_TYPE_GLASSFISH = "Glassfish";
    public static final String COMPONENT_TYPE_BINDING = "BindingComponent";
    public static final String SUN_JMS_BINDING = "sun-jms-binding";
    
    private static Alerter alertDel = new AlerterImpl();
    
    private AlertsUtil () {}
    
    public static Alerter getAlerter() {
        return alertDel;
    }    
    
    public static String getServerType() {
        return SERVER_TYPE_GLASSFISH;
    }
}
