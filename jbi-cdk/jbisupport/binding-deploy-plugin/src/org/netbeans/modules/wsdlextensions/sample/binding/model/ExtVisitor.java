/*
 * ExtVisitor.java
 */

package org.netbeans.modules.wsdlextensions.sample.binding.model;

/** 
 * This interface provides the visitor pattern to validate the extension model
 * objects.
 * 
 * @author chikkala
 */
public interface ExtVisitor {
    void visit(PortExt portExt);
}
