/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.glassfish.openesb.pojose.res.impl;

import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;
import org.glassfish.openesb.pojose.api.annotation.Endpoint;
import org.glassfish.openesb.pojose.core.util.Constants;
import org.w3c.dom.DocumentFragment;

/**
 *
 * @author gpatil
 */
public class ServiceEndpointImpl implements ServiceEndpoint {
    private ServiceEndpoint sep = null;
    private Endpoint anno = null;
    private volatile QName qname = null;
    private volatile boolean qnameEmpty = false;
    
    public ServiceEndpointImpl(ServiceEndpoint se, Endpoint anno){
        this.sep = se;
        this.anno = anno;
    }
        
    public DocumentFragment getAsReference(QName arg0) {
        return this.sep.getAsReference(arg0);
    }

    public String getEndpointName() {
        return this.sep.getEndpointName();
    }

    public QName[] getInterfaces() {
        return this.sep.getInterfaces();
    }

    public QName getServiceName() {
        return this.sep.getServiceName();
    }

    // Non ServiceEndpoint API methods
    public Endpoint getEndpointAnnotation(){
        return this.anno;
    }

    public ServiceEndpoint getServiceEndpoint(){
        return this.sep;
    }
    
    public synchronized boolean isQNameEmpty() {
        return qnameEmpty;
    }
    
    public synchronized QName getOperationQName(){
        if ((this.qname == null) && (!qnameEmpty)){
            String ns = "" ;
            String n = "" ;
            boolean ne = true;
            boolean nse = true;
            if (!Constants.ANNOTATION_NULL_VAL.equals(this.anno.operationQN())){
                this.qname = QName.valueOf(this.anno.operationQN());
                qnameEmpty = false;
            } else {
                if ((this.anno.interfaceNS() != null) && (!"".equals(anno.interfaceNS()))){
                    ns = anno.interfaceNS();
                    nse = false;
                }

                if ((this.anno.operationName() != null) && (!"".equals(anno.operationName()))){
                    n = anno.operationName();
                    ne = false;
                }

                if (ne && nse){
                    // both are empty.
                    qname = null;
                    qnameEmpty = true;
                } else {
                    qname = new QName(ns, n);
                    qnameEmpty = false;
                }
            }
        }
        return this.qname;
    }    
    
}
