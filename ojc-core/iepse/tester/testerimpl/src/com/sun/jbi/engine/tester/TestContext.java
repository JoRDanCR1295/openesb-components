/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.jbi.engine.tester;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.jbi.servicedesc.ServiceEndpoint;

/**
 *
 * @author radval
 */
public class TestContext {

//    private List<ServiceEndpoint> mDestinationEndpoints = new ArrayList<ServiceEndpoint>();
//    
//    private List<ServiceEndpoint> mReceivingEndpoints = new ArrayList<ServiceEndpoint>();
//    
    private Map<ServiceEndpoint, TestFolder> mServiceEndpointToTestFolderMap = new HashMap<ServiceEndpoint, TestFolder>();
    
    
//    public void addDestinationEndpoint(ServiceEndpoint endpoint) {
//        synchronized(this.mDestinationEndpoints) {
//            this.mDestinationEndpoints.add(endpoint);
//        }
//    }
//    
//    public boolean isDestinationEndpoint(ServiceEndpoint endpoint) {
//        synchronized(this.mDestinationEndpoints) {
//            return this.mDestinationEndpoints.contains(endpoint);
//        }
//    }
//    
//    
//    public void addReceivingEndpoint(ServiceEndpoint endpoint) {
//        synchronized(this.mDestinationEndpoints) {
//            this.mDestinationEndpoints.add(endpoint);
//        }
//    }
//    
//    public boolean isReceivingEndpoint(ServiceEndpoint endpoint) {
//        synchronized(this.mReceivingEndpoints) {
//            return this.mReceivingEndpoints.contains(endpoint);
//        }
//    }
//    
    public void addReceivingEndpoint(ServiceEndpoint endpoint, TestFolder folder) {
        synchronized(mServiceEndpointToTestFolderMap) {
            mServiceEndpointToTestFolderMap.put(endpoint, folder);
        }
    }
    
    
    
    public TestFolder getTestFolder(ServiceEndpoint endpoint) {
        synchronized(mServiceEndpointToTestFolderMap) {
            return mServiceEndpointToTestFolderMap.get(endpoint);
        }
    }
    
    
    
}
