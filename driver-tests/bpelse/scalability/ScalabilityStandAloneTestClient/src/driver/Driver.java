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
 * @(#)EngineChannel.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package driver;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Date;
import java.util.Properties;

import javax.xml.namespace.QName;
import javax.xml.ws.WebServiceRef;

import org.netbeans.enterprise.bpel.troubleticketwrapper.TroubleTicketService;

/**
 * @author mbhasin
 *
 */
public class Driver {

    public static final String THREADS = "threads";
    public static final String ASSIGNMENTLOAD = "assignmentLoad";
    public static final String BPELWAITDURATION = "bpelWaitDuration";
    public static final String PARTNERWAITDURATION = "partnerWaitDuration";
    public static final String SETOUTPUTREPLYMESSAGE = "setOutputReplyMessage";
    private static final String SERVICEURL = "serviceURL";
    private static final String ITERATIONS = "iterations";
    private static final String CORRELATEDMESSAGESENDDELAY = "correlatedMessageSendDelay";
    
    @WebServiceRef(wsdlLocation = "WEB-INF/wsdl/client/ScalabilityTest1/ScalabilityTest1.wsdl")
    //public TroubleTicketService service = new TroubleTicketService();
    public TroubleTicketService mService = null;
    
    private long mCorrelatedMessageSendDelay = -1; 
    
    private Properties mTestProp = null;

    private String testPropFile = "Driver.properties";
    
    private String mAssignmentLoad = null;
    private String mBpelWaitDuration = null;
    private String mPartnerWaitDuration = null;
    private boolean mSetOutput = false;
    private int threads = 1;
    private int iterations = 1;
    
    public Driver() {
        try {
            this.mTestProp = getTestProperties(testPropFile);
            System.out.println("Test properties : " + mTestProp);
            
            String serviceURL = mTestProp.getProperty(SERVICEURL);
            this.mService = createTroubleTicketService(serviceURL);
            
            this.mCorrelatedMessageSendDelay = Long.parseLong(mTestProp.getProperty(CORRELATEDMESSAGESENDDELAY)) * 1000;
            this.mAssignmentLoad = mTestProp.getProperty(ASSIGNMENTLOAD);
            this.mBpelWaitDuration = mTestProp.getProperty(BPELWAITDURATION);
            this.mPartnerWaitDuration = mTestProp.getProperty(PARTNERWAITDURATION);
            this.mSetOutput = Boolean.valueOf(mTestProp.getProperty(SETOUTPUTREPLYMESSAGE));
            this.threads = Integer.parseInt(mTestProp.getProperty(THREADS));
            this.iterations = Integer.parseInt(mTestProp.getProperty(ITERATIONS));
            
        } catch (MalformedURLException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
    
    private TroubleTicketService createTroubleTicketService(String serviceURL) {
        URL baseUrl;
        baseUrl = org.netbeans.enterprise.bpel.troubleticketwrapper.TroubleTicketService.class.getResource(".");
        URL url = null;
        try {
            url = new URL(baseUrl, serviceURL);
        } catch (MalformedURLException e) {
            e.printStackTrace();
        }
        QName serviceName = new QName("http://enterprise.netbeans.org/bpel/TroubleTicketWrapper", "TroubleTicketService");
        //service = new TroubleTicketService();
        TroubleTicketService service = new TroubleTicketService(url, serviceName);
        return service;
    }


    private Properties getTestProperties(String testPropFile) throws MalformedURLException, IOException {
        File file = new File(testPropFile);
        //InputStream is = Driver.class.getResourceAsStream(testPropFile);
        // the following is used instead of above one as this allows for reading the property file
        // outside of jar. This allows for easy changes to testing parameters without rebuilding the 
        // jar.
        InputStream is = file.toURL().openStream();
        Properties testProp = new Properties();
        try {
            testProp.load(is);
        } catch (IOException e) {
            e.printStackTrace();
        }
        return testProp;
    }

    public TroubleTicketService getService() {
        return mService;
    }

    public void setService(TroubleTicketService service) {
        mService = service;
    }

    public int getIterations() {
        return iterations;
    }

    public int getThreads() {
        return threads;
    }

    public String getAssignmentLoad() {
        return mAssignmentLoad;
    }

    public String getBpelWaitDuration() {
        return mBpelWaitDuration;
    }

    public String getPartnerWaitDuration() {
        return mPartnerWaitDuration;
    }
    
    public boolean isMSetOutput() {
        return mSetOutput;
    }

    private void runSample() throws Exception {
        
        Thread t1 = new FirstMesssageSenderThread(this);
        System.out.println(new Date().toString() + " Running test..");
        t1.start();
        
        Object monitor = new Object();
        
        if (mCorrelatedMessageSendDelay != -1 && mCorrelatedMessageSendDelay != 0) {
            synchronized (monitor) {
                monitor.wait(mCorrelatedMessageSendDelay);
            }
        }
        
        Thread t2 = new CorrelatedMesssageSenderThread(this);
        t2.start();

        t1.join();
        t2.join();
    }
    
    /**
     * @param args
     * @throws InterruptedException
     * @throws IOException 
     * @throws MalformedURLException 
     */
    public static void main(String[] args) {
        try {
            //System.out.println(new Date().toString() + " Running test..");
            new Driver().runSample();
            System.out.println(new Date().toString() + " Test Completed.");
        } catch (InterruptedException e) {
            e.printStackTrace();
        } catch (Exception e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

}