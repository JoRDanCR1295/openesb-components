/*
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://glassfish.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://glassfish.dev.java.net/public/CDDLv1.0.html.  
 *
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with 
 * your own identifying information: 
 * "Portions Copyrighted [year] [name of copyright owner]"
 */

/*
 * @(#)ListDestinations.java	1.6 08/01/06
 *
 * Copyright 2006 Sun Microsystems, Inc.  All Rights Reserved.
 */ 

import javax.management.*;
import javax.management.remote.*;
import com.sun.messaging.AdminConnectionFactory;
import com.sun.messaging.jms.management.server.MQObjectName;
import com.sun.messaging.jms.management.server.DestinationAttributes;
import com.sun.messaging.jms.management.server.DestinationOperations;

public class ListDestinations {
    public static void main(String[] args) {
	try  {
	    AdminConnectionFactory acf;

	    /*
	     * Create admin connection factory and connect to JMX Connector
	     * server using administrator username/password.
	     * A JMX connector client object is obtained from this.
	     */
	    acf = new AdminConnectionFactory();
	    JMXConnector jmxc = acf.createConnection("admin", "admin");

	    /*
	     * Get MBeanServer interface.
	     */
	    MBeanServerConnection mbsc = jmxc.getMBeanServerConnection();

	    /*
	     * Create object name of destination monitor mgr MBean.
	     */
	    ObjectName objName
		= new ObjectName(MQObjectName.DESTINATION_MANAGER_MONITOR_MBEAN_NAME);

	    ObjectName destinationObjNames[] = 
                (ObjectName[])mbsc.invoke(objName, DestinationOperations.GET_DESTINATIONS, null, null);

            System.out.println("Listing destinations:" );
	    for (int i = 0; i < destinationObjNames.length; ++i)  {
		ObjectName oneDestObjName = destinationObjNames[i];
		System.out.println("\tName: " + 
		    mbsc.getAttribute(oneDestObjName, DestinationAttributes.NAME));
		System.out.println("\tType: " + 
		    mbsc.getAttribute(oneDestObjName, DestinationAttributes.TYPE));
		System.out.println("\tState: " + 
		    mbsc.getAttribute(oneDestObjName, DestinationAttributes.STATE_LABEL));

		System.out.println("");
	    }

	    jmxc.close();
	} catch (Exception e)  {
	    e.printStackTrace();
	}
    }
}
