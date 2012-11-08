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
 * @(#)POJOClassMetadata.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package org.glassfish.openesb.pojose.core.anno.meta;

import java.util.Collections;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import javax.jbi.servicedesc.ServiceEndpoint;
import org.glassfish.openesb.pojose.api.Consumer;
import org.glassfish.openesb.pojose.api.annotation.ConsumerEndpoint;
import org.glassfish.openesb.pojose.api.annotation.Endpoint;
import org.glassfish.openesb.pojose.api.annotation.POJO;
import org.glassfish.openesb.pojose.api.annotation.Provider;
import org.glassfish.openesb.pojose.core.util.Util;
import org.glassfish.openesb.pojose.res.impl.ConsumerImpl;

public class POJOClassMetadata {
    private Class pojoClass; 
    private Provider provAnno;
    
    private OperationMetadata opm = null;
    private OnReplyMetadata onReply = null;
    private OnDoneMetadata onDone = null;
    private OnErrorMetadata onError = null;
    private OnFaultMetadata onFault = null;

    private Map<String, ConsumerEndpoint> field2ConsumerEndpoint = new ConcurrentHashMap<String, ConsumerEndpoint>();    
    private Map<String, ConsumerImpl> field2Consumer = new ConcurrentHashMap<String, ConsumerImpl>();    

    private boolean validOnDonePresent = false;
    private boolean valid2CallASynchInOut = false;
    private boolean valid2CallASynchInOnly = false;
    
    //@deprecated, soon to be removed
    private POJO pojoAnno;
    private Map<String, Endpoint> field2Endpoint = new ConcurrentHashMap<String, Endpoint>();
    private Map<String, ServiceEndpoint> field2SvcEndpoint = new ConcurrentHashMap<String, ServiceEndpoint>();
    
    public POJOClassMetadata() {
    }
    
    public void setOperationMetadata(OperationMetadata opnMetadata) {
        opm = opnMetadata;
    }
    
    public OperationMetadata getOperationMetadata() {
        return opm;
    }

    public void setOnReplyMetadata(OnReplyMetadata opnMetadata) {
        this.onReply = opnMetadata;
    }

    public OnReplyMetadata getOnReplyMetadata() {
        return this.onReply;
    }

    public void setOnDoneMetadata(OnDoneMetadata opnMetadata) {
        this.onDone = opnMetadata;
    }

    public OnDoneMetadata getOnDoneMetadata() {
        return this.onDone;
    }

    public void setOnErrorMetadata(OnErrorMetadata opnMetadata) {
        this.onError = opnMetadata;
    }

    public OnErrorMetadata getOnErrorMetadata() {
        return this.onError;
    }

    public void setOnFaultMetadata(OnFaultMetadata opnMetadata) {
        this.onFault = opnMetadata;
    }

    public OnFaultMetadata getOnFaultMetadata() {
        return this.onFault;
    }

    public POJO getPOJO() {
        return pojoAnno;
    }    
    
    public void setPojo(POJO p){
        pojoAnno = p;
    }

    public Provider getProvider() {
        return provAnno;
    }    
    
    public void setProvider(Provider p){
        provAnno = p;
    }
    
    public void setPojoClass(Class pc){
        this.pojoClass = pc;
    }
    
    public Class getPojoClass(){
        return this.pojoClass;
    }

    public Util.MEPStyle getMEPStyle(){
        Util.MEPStyle ret = Util.MEPStyle.InOut;
        // Message type based on return type of @OnDone method, if present else
        // @Operation. If return type is void, then message exchange is InOnly.
        // If return type is other than void then exchange type is InOut.
        if ((this.onDone != null) && (onDone.getMethod() != null)){
            if (onDone.getMethod().getReturnType() == Void.TYPE){
                ret = Util.MEPStyle.InOnly;
            }
        } else if ((this.opm != null) && (opm.getMethod() != null)){
            if (opm.getMethod().getReturnType() == Void.TYPE){
                ret = Util.MEPStyle.InOnly;
            }
        }
        return ret;
    }

    public boolean isAsynchConsumer(){
        return this.validOnDonePresent;
    }

    public boolean isValidOnDonePresent(){
        return this.validOnDonePresent;
    }

    public boolean isValidToCallASynchInOnly(){
        return this.valid2CallASynchInOnly;
    }

    public boolean isValidToCallASynchInOut(){
        return this.valid2CallASynchInOut;
    }

    public void addAllEndpoints(Map<String, Endpoint> map){
        this.field2Endpoint.putAll(map);
    }

    public void addAllConsumerEndpoints(Map<String, ConsumerEndpoint> map){
        this.field2ConsumerEndpoint.putAll(map);
    }

    public void addConsumer(String fldName, ConsumerImpl cons){
        this.field2Consumer.put(fldName, cons);
    }
    
    public void addServiceEndpoint(String fldName, ServiceEndpoint se){
        this.field2SvcEndpoint.put(fldName, se);
    }

    public ServiceEndpoint getServiceEndpoint(String fldName){
        return this.field2SvcEndpoint.get(fldName);
    }

    public Consumer getConsumer(String fldName){
        return this.field2Consumer.get(fldName).clone();
    }
    
    public boolean isFieldForConsumer(String fn){
        return this.field2ConsumerEndpoint.containsKey(fn);
    }
    public Map<String, Endpoint> getField2EndpointMap(){
        return Collections.unmodifiableMap(this.field2Endpoint);
    }
    
    public Map<String, ConsumerEndpoint> getField2ConsumerEndpointMap(){
        return Collections.unmodifiableMap(this.field2ConsumerEndpoint);
    }

    public void computeValues(){
        if ((this.onDone != null)&& (this.onDone.getMethod() != null)){
            this.validOnDonePresent = true;
            this.valid2CallASynchInOnly = true;
            
            if ((this.onReply != null) && (this.onReply.getMethod() != null)){
                this.valid2CallASynchInOut = true;
            }
        }
    }
}
