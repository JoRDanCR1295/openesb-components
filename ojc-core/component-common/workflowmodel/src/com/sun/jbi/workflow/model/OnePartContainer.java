/*
 * OnePartContainer.java
 * 
 * Created on Jun 27, 2007, 2:25:18 PM
 * 
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.jbi.workflow.model;

import javax.wsdl.Part;

/**
 *
 * @author radval
 */
public interface OnePartContainer extends ModelElement {

    Part  getWSDLPart() throws ModelException;
    
    String getPart ();
    
    PortTypeModelElement getPortTypeElement ();
}
