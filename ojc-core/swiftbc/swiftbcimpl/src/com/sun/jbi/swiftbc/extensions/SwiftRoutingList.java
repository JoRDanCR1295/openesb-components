/*
 * RoutingList.java
 *
 * Created on April 7, 2007, 7:03 PM
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package com.sun.jbi.swiftbc.extensions;

/**
 *
 * @author Sun Microsystems, Inc.
 */
public interface SwiftRoutingList {
    public int getNumberOfStep();
    public SwiftRoutingStep getRoutingStep(int iStep);
    public void insert(int iStep,SwiftRoutingStep routingStep);
    public void destroy(int iStep);
    
}
