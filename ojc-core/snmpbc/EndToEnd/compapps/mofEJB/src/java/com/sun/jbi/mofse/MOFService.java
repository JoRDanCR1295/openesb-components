/*
 * MOFService.java
 * 
 * Created on Apr 14, 2007, 6:16:28 PM
 * 
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package com.sun.jbi.mofse;

import com.sun.soabi.snmpbc.metadataquery.NetworkElementIdent;
import com.sun.soabi.snmpbc.metadataresponse.MetaDataResponse;
import com.sun.soabi.snmpbc.metadataresponse.NetworkElementProperties;
import java.util.logging.Logger;
import javax.ejb.Stateless;
import javax.jws.WebService;
import org.netbeans.j2ee.wsdl.mofwsdl.MofWSDLPortType;

/**
 *
 * @author echou
 */
@WebService(serviceName = "mofWSDLService", portName = "mofWSDLPort", endpointInterface = "org.netbeans.j2ee.wsdl.mofwsdl.MofWSDLPortType", targetNamespace = "http://j2ee.netbeans.org/wsdl/mofWSDL", wsdlLocation = "META-INF/wsdl/MOFService/mofWSDL.wsdl")
@Stateless
public class MOFService implements MofWSDLPortType {

    private static final Logger logger = Logger.getLogger(MOFService.class.getName());
    
    private MOFDatabase mofDB;
    
    /** Creates a new instance of MOFServiceEngine */
    public MOFService() throws Exception {
        mofDB = MOFDatabase.getInstance();
    }
    
    public static final int NPROCESSINGENGINES =  2;

    public com.sun.soabi.snmpbc.metadataresponse.MetaDataResponse mofWSDLOperation(com.sun.soabi.snmpbc.metadataquery.MetaDataQuery q) {
        MetaDataResponse ret = new MetaDataResponse();
        ret.setQuereyID(q.getQueryID());
        
        for (NetworkElementIdent qi : q.getNetworkElementIdents()) {
            //String v = qi.getIPAddress() + ":" + qi.getPort();
            //System.out.println("MOF: v = " + v);
            //int h = v.hashCode();
            int procEngId = 1 + qi.getPort() % NPROCESSINGENGINES;
            
            NetworkElementProperties retel = new NetworkElementProperties();
            retel.setReplyID(qi.getReplyID());
            retel.getProcessorID().add("Adaptation" + procEngId);
            ret.getNetworkElements().add(retel);
        }

        return ret;
    }

}
