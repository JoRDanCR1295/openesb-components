/*
 * RoutingStep.java
 *
 * Created on April 7, 2007, 7:04 PM
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package com.sun.jbi.swiftbc.extensions;

/**
 *
 * @author Sun Microsystems, Inc.
 */
public interface SwiftRoutingStep {
    public void setVisit(int visit);
    public int getVisit();
    public void setPlugIn(String sPlugIn);
    public String getPlugIn();
    public void setPrimitive(String sPrimitive);
    public String getPrimitive();
    
}
