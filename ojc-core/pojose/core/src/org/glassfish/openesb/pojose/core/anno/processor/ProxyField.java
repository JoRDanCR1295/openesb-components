/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.glassfish.openesb.pojose.core.anno.processor;

/**
 *
 * @author gpatil
 */
public interface ProxyField {
    public String getName();
    public String getFieldClassName();
    public ProxyResourceAnnotation getResourceAnnotation();    
}
