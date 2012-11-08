/*
 * MockRoutingStep.java
 *
 * Created on April 7, 2007, 10:53 PM
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package com.sun.jbi.swiftbc.extensions.mock;

import com.sun.jbi.swiftbc.extensions.SwiftRoutingStep;

/**
 *
 * @author Sun Microsystems, Inc.
 */
public class MockRoutingStep implements SwiftRoutingStep{
    private int visit = 0;
    private String primitive;
    private String plugIn;
    /** Creates a new instance of MockRoutingStep */
    public MockRoutingStep() {
    }

    public void setPrimitive(String sPrimitive) {
        primitive = sPrimitive;
    }

    public void setPlugIn(String sPlugIn) {
        plugIn = plugIn;
    }

    public void setVisit(int visit) {
        this.visit = visit;
    }

    public int getVisit() {
        return visit;
    }

    public String getPrimitive() {
        return primitive;
    }

    public String getPlugIn() {
        return plugIn;
    }
    
}
