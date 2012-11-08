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
 * @(#)DestListMetrics.java	1.5 08/01/06
 *
 * Copyright 2006 Sun Microsystems, Inc.  All Rights Reserved.
 */ 

import java.util.Enumeration;
import java.util.Properties;
import java.util.Hashtable;
import javax.jms.*;

/**
 * The DestListMetrics example is a JMS application that monitors the
 * destinations on a Sun Java(tm) System Message Queue broker. It does so by 
 * subscribing to a topic named 'mq.metrics.destination_list'. The 
 * messages that arrive contain information describing the 
 * destinations that currently exist on the broker such as:
 *  - destination name
 *  - destination type
 *  - whether the destination is temporary or not
 *
 * By default DestListMetrics will connect to the broker running on localhost:7676.
 * You can use -DimqAddressList attribute to change the host, port and 
 * transport:
 *
 *	java -DimqAddressList=mq://<host>:<port>/jms DestListMetrics
 */
public class DestListMetrics implements MessageListener  {
    ConnectionFactory        metricConnectionFactory;
    Connection               metricConnection;
    Session                  metricSession;
    MessageConsumer          metricConsumer;
    Topic                    metricTopic;
    MetricsPrinter           mp;
    String                   metricTopicName = null;
    int                      rowsPrinted = 0;
  
    public static void main(String args[])  {

	DestListMetrics dlm = new DestListMetrics();

        dlm.initPrinter();
        dlm.initJMS();
        dlm.subscribeToMetric();
    }

    public DestListMetrics() {
    }

    /*
     * Initializes the class that does the printing, MetricsPrinter.
     * See the MetricsPrinter class for details.
     */
    private void initPrinter() {
	String oneRow[] = new String[ 3 ];
	int i = 0;

	mp = new MetricsPrinter(3, 2, "-");
	oneRow[i++] = "Destination Name";
	oneRow[i++] = "Type";
	oneRow[i++] = "Is Temporary";
	mp.addTitle(oneRow);
    }

    /** 
     * Create the Connection and Session etc.
     */
    public void initJMS() {
        try {
            metricConnectionFactory = new com.sun.messaging.ConnectionFactory();
            metricConnection = metricConnectionFactory.createConnection();
            metricConnection.start();

            //  creating Session
            //	Transaction Mode: None
            //	Acknowledge Mode: Automatic
            metricSession = metricConnection.createSession(false,
				Session.AUTO_ACKNOWLEDGE);
        } catch(Exception e) {
            System.err.println("Cannot create metric connection or session: "
			+ e.getMessage());
            e.printStackTrace();
	    System.exit(1);
        }
    }
  
    public void subscribeToMetric() {
        try {

	    metricTopicName = "mq.metrics.destination_list";
            metricTopic = metricSession.createTopic(metricTopicName);

            metricConsumer = metricSession.createConsumer(metricTopic);
            metricConsumer.setMessageListener(this);
        } catch(JMSException e) {
            System.err.println("Cannot subscribe to metric topic: "
			+ e.getMessage());
            e.printStackTrace();
	    System.exit(1);
        }
    }

    /*
     * When a metric message arrives
     *	- verify it's type
     *	- extract it's fields
     *  - print one row of output
     */
    public void onMessage(Message m)  {
	try  {
	    MapMessage mapMsg = (MapMessage)m;
	    String type = mapMsg.getStringProperty("type");

	    if (type.equals(metricTopicName))  {
	        String oneRow[] = new String[ 3 ];

	        /*
	         * Extract metrics
	         */
                for (Enumeration e = mapMsg.getMapNames();
                         e.hasMoreElements();) {

		    String metricDestName = (String)e.nextElement();
		    Hashtable destValues = (Hashtable)mapMsg.getObject(metricDestName);
		    int i = 0;

		    oneRow[i++] = (destValues.get("name")).toString();
		    oneRow[i++] = (destValues.get("type")).toString();
		    oneRow[i++] = (destValues.get("isTemporary")).toString();

		    mp.add(oneRow);
		}

		mp.print();
		System.out.println("");

		mp.clear();
	    } else  {
	        System.err.println("Msg received: not destination list metric type");
	    }
	} catch (Exception e)  {
	    System.err.println("onMessage: Exception caught: " + e);
	}
    }
}
