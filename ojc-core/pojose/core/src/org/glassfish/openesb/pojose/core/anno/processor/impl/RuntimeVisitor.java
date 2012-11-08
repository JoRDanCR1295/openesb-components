/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.glassfish.openesb.pojose.core.anno.processor.impl;

import org.glassfish.openesb.pojose.core.anno.processor.visitor.ValidationVisitor;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.glassfish.openesb.pojose.api.annotation.ConsumerEndpoint;
import org.glassfish.openesb.pojose.api.annotation.Endpoint;
import org.glassfish.openesb.pojose.api.annotation.POJO;
import org.glassfish.openesb.pojose.api.annotation.Provider;
import org.glassfish.openesb.pojose.core.anno.meta.OnDoneMetadata;
import org.glassfish.openesb.pojose.core.anno.meta.OnErrorMetadata;
import org.glassfish.openesb.pojose.core.anno.meta.OnFaultMetadata;
import org.glassfish.openesb.pojose.core.anno.meta.OnReplyMetadata;
import org.glassfish.openesb.pojose.core.anno.meta.OperationMetadata;
import org.glassfish.openesb.pojose.core.anno.meta.POJOClassMetadata;
import org.glassfish.openesb.pojose.core.anno.processor.POJOAnnotationProcessor;
import org.glassfish.openesb.pojose.core.anno.processor.ProxyClass;
import org.glassfish.openesb.pojose.core.anno.processor.ProxyMethod;
import org.glassfish.openesb.pojose.core.anno.processor.ProxyOnDoneAnnotation;
import org.glassfish.openesb.pojose.core.anno.processor.ProxyOnErrorAnnotation;
import org.glassfish.openesb.pojose.core.anno.processor.ProxyOnFaultAnnotation;
import org.glassfish.openesb.pojose.core.anno.processor.ProxyOnReplyAnnotation;
import org.glassfish.openesb.pojose.core.anno.processor.ProxyOperationAnnotation;
import org.glassfish.openesb.pojose.core.anno.processor.ProxyPOJOAnnotation;
import org.glassfish.openesb.pojose.core.anno.processor.ProxyProviderAnnotation;
import org.glassfish.openesb.pojose.core.util.I18n;

/**
 *
 * @author gpatil
 */
public class RuntimeVisitor extends ValidationVisitor{
    private List<POJOClassMetadata> list = new ArrayList<POJOClassMetadata>();

    private static Logger logger = Logger.getLogger(POJOAnnotationProcessor.class.getName());

    @Override
    public void visitClass(ProxyClass c) {
        super.visitClass(c);
        POJOClassMetadata pm = getPojoMeta(c);
        if (pm != null){
            this.list.add(pm);
        }
    }

    // ***** Non API methods.
    public List<POJOClassMetadata> getPOJOClassMetadata(){
        return this.list;
    }


    private POJOClassMetadata getPojoMeta(ProxyClass cls) {
        POJOClassMetadata pmd = null;
        ProxyProviderAnnotation ppd = cls.getAnnotationProvider();

        if (ppd != null){
            pmd = parseAnnoProvider(cls, ppd);
        } else {
            ProxyPOJOAnnotation ppojo = cls.getAnnotationPOJO();
            if (ppojo != null) {
                pmd = parseAnnoPOJO(cls, ppojo);
            }
        }

        return pmd;
    }

    private POJOClassMetadata parseAnnoProvider(ProxyClass cls, ProxyProviderAnnotation ppd) {
        POJOClassMetadata pmd = null;
        OperationMetadata opnMd = null;
        OnReplyMetadata orMd = null;
        OnDoneMetadata odMd = null;
        OnErrorMetadata oeMd = null;
        OnFaultMetadata ofMd = null;
        ProxyOperationAnnotation op = null;
        ProxyOnDoneAnnotation od = null;
        ProxyOnReplyAnnotation or = null;
        ProxyOnErrorAnnotation oe = null;
        ProxyOnFaultAnnotation of = null;

        int opCtr = 0;
        int odCtr = 0;
        int orCtr = 0;
        int oeCtr = 0;
        int ofCtr = 0;

        if (logger.isLoggable(Level.FINE)){
            String msg = I18n.lf("POJOSE-1005: POJO class found {0}", cls.getName()); //NOI18N
            logger.log(Level.FINE, msg);
        }

        List<ProxyMethod> ms = cls.getMethods();
        for (ProxyMethod m : ms) {
            op = m.getOperationAnnotation();
            // Keep it message oriented, only one operation per POJO/Endpoint.
            if ((op != null) && (opCtr == 0)){
                opCtr++;
                opnMd = new OperationMetadata();
                opnMd.setMethodName(m.getName());
                opnMd.setOperation(((WrapperOperation) op).getAnnotation());
                opnMd.setMethod(((WrapperMethod) m).getMethod());

                opnMd.setMethodParameterType(m.getFirstParameterType());
                opnMd.setMethodReturnType(m.getReturnParameterType());

                opnMd.setOperationName(op.name());

                if (logger.isLoggable(Level.FINE)){
                    String msg = I18n.lf("POJOSE-1006: Operation found \"{0}\", other operations will be ignored.", m.getName()); //NOI18N
                    logger.log(Level.FINE, msg);
                }
            }

            od = m.getOnDoneAnnotation();
            // Keep it message oriented, only one operation per POJO/Endpoint.
            if ((od != null) && (odCtr == 0)){
                odCtr++;
                odMd = new OnDoneMetadata();
                odMd.setMethodName(m.getName());
                odMd.setOnDone(((WrapperOnDone) od).getAnnotation());
                odMd.setMethod(((WrapperMethod) m).getMethod());

                //odMd.setMethodParameterType(m.getFirstParameterType());
                odMd.setMethodReturnType(m.getReturnParameterType());
            }

            or = m.getOnReplyAnnotation();
            // Keep it message oriented, only one operation per POJO/Endpoint.
            if ((or != null) && (orCtr == 0)){
                orCtr++;
                orMd = new OnReplyMetadata();
                orMd.setMethodName(m.getName());
                orMd.setOnReply(((WrapperOnReply) or).getAnnotation());
                orMd.setMethod(((WrapperMethod) m).getMethod());

                orMd.setMethodParameterType(m.getFirstParameterType());
                orMd.setMethodReturnType(m.getReturnParameterType());
            }

            oe = m.getOnErrorAnnotation();
            // Keep it message oriented, only one operation per POJO/Endpoint.
            if ((oe != null) && (oeCtr == 0)){
                oeCtr++;
                oeMd = new OnErrorMetadata();
                oeMd.setMethodName(m.getName());
                oeMd.setOnError(((WrapperOnError) oe).getAnnotation());
                oeMd.setMethod(((WrapperMethod) m).getMethod());

                oeMd.setMethodParameterType(m.getFirstParameterType());
                oeMd.setMethodReturnType(m.getReturnParameterType());
            }

            of = m.getOnFaultAnnotation();
            // Keep it message oriented, only one operation per POJO/Endpoint.
            if ((of != null) && (ofCtr == 0)){
                ofCtr++;
                ofMd = new OnFaultMetadata();
                ofMd.setMethodName(m.getName());
                ofMd.setOnFault(((WrapperOnFault) of).getAnnotation());
                ofMd.setMethod(((WrapperMethod) m).getMethod());

                ofMd.setMethodParameterType(m.getFirstParameterType());
                ofMd.setMethodReturnType(m.getReturnParameterType());
            }
        }

        //XXX TODO
        //Add code to count and validation for opCtr, odCtr, orCtr.


        // Is valid only when POJO and Operation annotation present
        if (opnMd != null) {
            WrapperClass jc = (WrapperClass) cls;
            WrapperProvider wppd = (WrapperProvider) ppd;
            Provider prov = wppd.getProviderAnnotation();
            pmd = new POJOClassMetadata();
            pmd.setProvider(prov);
            pmd.setPojoClass(jc.getPOJOClass());
            pmd.setOperationMetadata(opnMd);
            pmd.setOnReplyMetadata(orMd);
            pmd.setOnDoneMetadata(odMd);
            pmd.setOnErrorMetadata(oeMd);
            pmd.setOnFaultMetadata(ofMd);
            if (logger.isLoggable(Level.FINE)){
                String msg = I18n.lf("POJOSE-3001: Provisioning endpoint {0}.", ppd); //NOI18N
                logger.log(Level.FINE, msg);
            }

            Map<String, ConsumerEndpoint> eps = jc.getField2ConsumerEndpointMap();
            pmd.addAllConsumerEndpoints(eps);
            pmd.computeValues();
        } else {
            String msg = I18n.loc("POJOSE-7004: No public method with Operation annotation found in class {0}, ignoring this class.", cls.getName());
            logger.log(Level.SEVERE, msg);
        }
        return pmd;
    }

    private POJOClassMetadata parseAnnoPOJO(ProxyClass cls, ProxyPOJOAnnotation ppojo) {
        POJOClassMetadata pmd = null;
        OperationMetadata opnMd = null;
        ProxyOperationAnnotation op = null;

        if (logger.isLoggable(Level.FINE)){
            String msg = I18n.lf("POJOSE-1005: POJO class found {0}", cls.getName()); //NOI18N
            logger.log(Level.FINE, msg);
        }

        List<ProxyMethod> ms = cls.getMethods();
        for (ProxyMethod m : ms) {
            op = m.getOperationAnnotation();
            if (op != null) {
                opnMd = new OperationMetadata();
                opnMd.setMethodName(m.getName());
                opnMd.setOperation(((WrapperOperation) op).getAnnotation());
                opnMd.setMethod(((WrapperMethod) m).getMethod());

                opnMd.setMethodParameterType(m.getFirstParameterType());
                opnMd.setMethodReturnType(m.getReturnParameterType());

                opnMd.setOperationName(op.name());

                if (logger.isLoggable(Level.FINE)){
                    String msg = I18n.lf("POJOSE-1006: Operation found \"{0}\", other operations will be ignored.", m.getName()); //NOI18N
                    logger.log(Level.FINE, msg);
                }
                // Keep it message oriented, only one operation per POJO/Endpoint.
                break;
            }
        }

        // Is valid only when POJO and Operation annotation present
        if (opnMd != null) {
            WrapperClass jc = (WrapperClass) cls;
            WrapperPOJO jPojo = (WrapperPOJO) ppojo;
            POJO pojo = jPojo.getPOJOAnnotation();
            pmd = new POJOClassMetadata();
            pmd.setPojo(pojo);
            pmd.setPojoClass(jc.getPOJOClass());
            pmd.setOperationMetadata(opnMd);

            if (logger.isLoggable(Level.FINE)){
                String msg = I18n.lf("POJOSE-3001: Provisioning endpoint {0}.", ppojo); //NOI18N
                logger.log(Level.FINE, msg);
            }

            Map<String, Endpoint> eps = jc.getField2EndpointMap();
            pmd.addAllEndpoints(eps);

            Map<String, ConsumerEndpoint> eps1 = jc.getField2ConsumerEndpointMap();
            pmd.addAllConsumerEndpoints(eps1);
            pmd.computeValues();
        } else {
            String msg = I18n.loc("POJOSE-7004: No public method with Operation annotation found in class {0}, ignoring this class.", cls.getName());
            logger.log(Level.SEVERE, msg);
        }
        return pmd;
    }
}
