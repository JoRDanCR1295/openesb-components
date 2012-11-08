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
 * @(#)BrokerMetrics.java	1.5 08/01/06
 *
 * Copyright 2006 Sun Microsystems, Inc.  All Rights Reserved.
 */ 

import java.util.Enumeration;
import java.util.Properties;
import javax.jms.*;

/**
 * The BrokerMetrics example is a JMS application that monitors the
 * Sun Java(tm) System Message Queue broker. It does so by subscribing to a
 * topic named 'mq.metrics.broker'. The messages that arrive contain
 * Broker metric information such as number of messages flowing in/out 
 * of the broker, size in bytes of messages flowing in/out etc.
 *
 * This application also shows how to use the timestamp in each metric
 * message to calculate rates e.g. rate at which messages are flowing
 * into the broker.
 *
 * This application takes the following arguments:
 *	-m ttl		Show totals e.g. total/accumulative number of msgs
 *			that flowed in/out of the broker. This is the
 *			default.
 *	-m rts		Show rates e.g. rate of mesasge flow in/out of 
 *			broker.
 *
 * By default BrokerMetrics will connect to the broker running on localhost:7676.
 * You can use -DimqAddressList attribute to change the host, port and 
 * transport:
 *
 *	java -DimqAddressList=mq://<host>:<port>/jms DestListMetrics
*
 */
public class BrokerMetrics implements MessageListener  {
    ConnectionFactory        metricConnectionFactory;
    Connection               metricConnection;
    Session                  metricSession;
    MessageConsumer          metricConsumer;
    Topic                    metricTopic;
    MetricsPrinter           mp;
    int                      rowsPrinted = 0;
    boolean                  showTotals = true;
    MapMessage               previous = null;
  
    public static void main(String args[])  {
	boolean		totals = true;

	for (int i = 0; i < args.length; ++i)  {
            if (args[i].equals("-m"))  {
		String type = args[i+1];

		if (type.equals("ttl"))  {
	            totals = true;
		} else if (type.equals("rts"))  {
	            totals = false;
		}
	    }
	}
	BrokerMetrics bm = new BrokerMetrics();

	bm.showTotals = totals;
        bm.initPrinter();
        bm.initJMS();
        bm.subscribeToMetric();
    }

    /*
     * Initializes the class that does the printing, MetricsPrinter.
     * See the MetricsPrinter class for details.
     */
    private void initPrinter() {
	String oneRow[] = new String[ 8 ];
	int    span[] = new int[ 8 ];
	int i = 0;

	mp = new MetricsPrinter(8, 2, "-", MetricsPrinter.CENTER);
	mp.setTitleAlign(MetricsPrinter.CENTER);

	span[i++] = 2;
	span[i++] = 0;
	span[i++] = 2;
	span[i++] = 0;
	span[i++] = 2;
	span[i++] = 0;
	span[i++] = 2;
	span[i++] = 0;

	i = 0;
	if (showTotals)  {
	    oneRow[i++] = "Msgs";
	    oneRow[i++] = "";
	    oneRow[i++] = "Msg Bytes";
	    oneRow[i++] = "";
	    oneRow[i++] = "Pkts";
	    oneRow[i++] = "";
	    oneRow[i++] = "Pkt Bytes";
	    oneRow[i++] = "";
	} else  {
	    oneRow[i++] = "Msgs/sec";
	    oneRow[i++] = "";
	    oneRow[i++] = "Msg Bytes/sec";
	    oneRow[i++] = "";
	    oneRow[i++] = "Pkts/sec";
	    oneRow[i++] = "";
	    oneRow[i++] = "Pkt Bytes/sec";
	    oneRow[i++] = "";
	}
	mp.addTitle(oneRow, span);

	i = 0;
	oneRow[i++] = "In";
	oneRow[i++] = "Out";
	oneRow[i++] = "In";
	oneRow[i++] = "Out";
	oneRow[i++] = "In";
	oneRow[i++] = "Out";
	oneRow[i++] = "In";
	oneRow[i++] = "Out";

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
            metricTopic = metricSession.createTopic("mq.metrics.broker");

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

	    if (type.equals("mq.metrics.broker"))  {
		if (showTotals)  {
		    doTotals(mapMsg);
		} else  {
		    doRates(mapMsg);
		}

	    } else  {
	        System.err.println("Msg received: not broker metric type");
	    }
	} catch (Exception e)  {
	    System.err.println("onMessage: Exception caught: " + e);
	}
    }

    private void doTotals(MapMessage mapMsg)  {
	try  {
	    String oneRow[] = new String[ 8 ];
	    int i = 0;

	    /*
	     * Extract broker metrics
	     */
	    oneRow[i++] = Long.toString(mapMsg.getLong("numMsgsIn"));
	    oneRow[i++] = Long.toString(mapMsg.getLong("numMsgsOut"));
	    oneRow[i++] = Long.toString(mapMsg.getLong("msgBytesIn"));
	    oneRow[i++] = Long.toString(mapMsg.getLong("msgBytesOut"));
	    oneRow[i++] = Long.toString(mapMsg.getLong("numPktsIn"));
	    oneRow[i++] = Long.toString(mapMsg.getLong("numPktsOut"));
	    oneRow[i++] = Long.toString(mapMsg.getLong("pktBytesIn"));
	    oneRow[i++] = Long.toString(mapMsg.getLong("pktBytesOut"));

	    mp.add(oneRow);

	    if ((rowsPrinted % 20) == 0)  {
	        mp.print();
	    } else  {
	        mp.print(false);
	    }

	    rowsPrinted++;

	    mp.clear();
	} catch (Exception e)  {
	    System.err.println("onMessage: Exception caught: " + e);
	}
    }

    private void doRates(MapMessage mapMsg)  {
	String oneRow[] = new String[ 8 ];
	int i = 0;
	
	try  {
	    if (previous == null)  {
	        oneRow[i++] = "0";
	        oneRow[i++] = "0";
	        oneRow[i++] = "0";
	        oneRow[i++] = "0";
	        oneRow[i++] = "0";
	        oneRow[i++] = "0";
	        oneRow[i++] = "0";
	        oneRow[i++] = "0";
	    } else  {
	        long prevVal, newVal, prevSecs, newSecs, tmp;
		float secs;

	        prevSecs = previous.getLongProperty("timestamp");
	        newSecs = mapMsg.getLongProperty("timestamp");
		secs = (float)(newSecs - prevSecs)/(float)1000;

	        oneRow[i++] = Long.toString(getRate(previous, mapMsg, secs, "numMsgsIn"));
	        oneRow[i++] = Long.toString(getRate(previous, mapMsg, secs, "numMsgsOut"));
	        oneRow[i++] = Long.toString(getRate(previous, mapMsg, secs, "msgBytesIn"));
	        oneRow[i++] = Long.toString(getRate(previous, mapMsg, secs, "msgBytesOut"));
	        oneRow[i++] = Long.toString(getRate(previous, mapMsg, secs, "numPktsIn"));
	        oneRow[i++] = Long.toString(getRate(previous, mapMsg, secs, "numPktsOut"));
	        oneRow[i++] = Long.toString(getRate(previous, mapMsg, secs, "pktBytesIn"));
	        oneRow[i++] = Long.toString(getRate(previous, mapMsg, secs, "pktBytesOut"));
	    }

	    previous = mapMsg;

	    mp.add(oneRow);

	    if ((rowsPrinted % 20) == 0)  {
	        mp.print();
	    } else  {
	        mp.print(false);
	    }

	    rowsPrinted++;

	    mp.clear();
	} catch (Exception e)  {
	    System.err.println("onMessage: Exception caught: " + e);
	    e.printStackTrace();
	}
    }

    private long getRate(MapMessage previous, MapMessage mapMsg, float secs,
				String propName) throws JMSException {

        long prevVal, newVal, prevSecs, newSecs, tmp;

	prevVal = previous.getLong(propName);
	newVal = mapMsg.getLong(propName);

	tmp = (long)((newVal - prevVal)/secs);

	return (tmp);
    }
}
