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
 * @(#)POJOAnnotationProcessor.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package org.glassfish.openesb.pojose.core.anno.processor;


import org.glassfish.openesb.pojose.core.anno.processor.visitor.Visitor;
import java.io.File;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jbi.servicedesc.ServiceEndpoint;
import org.glassfish.openesb.pojose.api.annotation.ConsumerEndpoint;
import org.glassfish.openesb.pojose.api.annotation.Endpoint;
import org.glassfish.openesb.pojose.api.annotation.POJOResource;
import org.glassfish.openesb.pojose.api.annotation.Resource;
import org.glassfish.openesb.pojose.core.anno.meta.POJOClassMetadata;
import org.glassfish.openesb.pojose.core.util.I18n;
import org.glassfish.openesb.pojose.res.impl.CallTracker;
import org.glassfish.openesb.pojose.res.impl.ConsumerImpl;
import org.glassfish.openesb.pojose.res.impl.ContextImpl;
import org.glassfish.openesb.pojose.res.impl.POJOContextImpl;

/**
 * Process POJO SE annotation.
 *
 * @author gpatil
 */
public class POJOAnnotationProcessor {
    private static Logger logger = Logger.getLogger(POJOAnnotationProcessor.class.getName());

    private POJOAnnotationProcessor() {
    }

    public static void refreshEndpoints(String root, ProxyClassLoader cl,
            Visitor visitor){
        PojoClassScanner pcs = new PojoClassScanner(cl);
        pcs.scan(new File(root), visitor);
        return;
    }

   public static void injectFields(POJOClassMetadata meta,
           Object pojo, ContextImpl ctx, CallTracker pt){
        Class pc = meta.getPojoClass();
        Field[] fields = pc.getDeclaredFields();
        for (Field f: fields){
            Resource resCtx = (Resource) f.getAnnotation(Resource.class);
            if (resCtx != null){
                setField(f, pojo, ctx);
                if (logger.isLoggable(Level.FINE)){
                    String msg = I18n.lf("POJOSE-1000: Injected POJOContext into object {0} of class {1}", pojo, pc); //NOI18N
                    logger.fine(msg);
                }
            } else {
                if (f.getAnnotation(ConsumerEndpoint.class) != null){
                    ConsumerImpl consumer = (ConsumerImpl) meta.getConsumer(f.getName());
                    if (consumer != null){
                        consumer.setProviderME(ctx.getMessageExchange());
                        consumer.setCallTracker(pt);
                        setField(f, pojo, consumer);
                        if (logger.isLoggable(Level.FINE)){
                            String msg = I18n.lf("POJOSE-1002: Injected ServiceEndpoint into object {0} of class {1}", pojo, consumer); //NOI18N
                            logger.fine(msg);
                        }
                    } else {
                        // We will never be here..should not be here in case we are here...
                        String msg = I18n.loc("POJOSE-7003: Error, ServiceEndpoint got is null, not injecting field {1}.", f.getName());
                        logger.severe(msg);
                    }
                }
            }
        }
    }

   /**
    * @deprecated  Will be removed once support for old annotation is removed.
    *
    * @param meta
    * @param ctx
    * @param pjctx
    * @param pojo
    */
   public static void injectFields(POJOClassMetadata meta, ContextImpl ctx,
           POJOContextImpl pjctx, Object pojo){
        Class pc = meta.getPojoClass();
        Field[] fields = pc.getDeclaredFields();
        for (Field f: fields){

            Resource resCtx = (Resource) f.getAnnotation(Resource.class);
            if (resCtx != null){
                setField(f, pojo, ctx);
                if (logger.isLoggable(Level.FINE)){
                    String msg = I18n.lf("POJOSE-1000: Injected POJOContext into object {0} of class {1}", pojo, pc); //NOI18N
                    logger.fine(msg);
                }
            } else {
                if (f.getAnnotation(ConsumerEndpoint.class) != null){
                    ConsumerImpl consumer = (ConsumerImpl) meta.getConsumer(f.getName());
                    if (consumer != null){
                        consumer.setProviderME(ctx.getMessageExchange());
                        setField(f, pojo, consumer);
                        if (logger.isLoggable(Level.FINE)){
                            String msg = I18n.lf("POJOSE-1002: Injected ServiceEndpoint into object {0} of class {1}", pojo, consumer); //NOI18N
                            logger.fine(msg);
                        }
                    } else {
                        // We will never be here..should not be here in case we are here...
                        String msg = I18n.loc("POJOSE-7003: Error, ServiceEndpoint got is null, not injecting field {1}.", f.getName());
                        logger.severe(msg);
                    }
                } else {
                    POJOResource pojoctx = (POJOResource) f.getAnnotation(POJOResource.class);
                    if (pojoctx != null) {
                        setField(f, pojo, pjctx);
                        if (logger.isLoggable(Level.FINE)){
                            String msg = I18n.lf("POJOSE-1000: Injected POJOContext into object {0} of class {1}", pojo, pc); //NOI18N
                            logger.fine(msg);
                        }
                    } else {
                        if (f.getAnnotation(Endpoint.class) != null){
                            ServiceEndpoint se = meta.getServiceEndpoint(f.getName());
                            if (se != null){
                                setField(f, pojo, se);
                                if (logger.isLoggable(Level.FINE)){
                                    String msg = I18n.lf("POJOSE-1002: Injected ServiceEndpoint into object {0} of class {1}", pojo, se); //NOI18N
                                    logger.fine(msg);
                                }
                            } else {
                                // We will never be here..should not be here in case we are here...
                                String msg = I18n.loc("POJOSE-7003: Error, ServiceEndpoint got is null, not injecting field {1}.", f.getName());
                                logger.severe(msg);
                            }
                        }
                    }
                }
            }
        }
    }

    private static void setField(Field field, Object target, Object value) {
        if (!Modifier.isPublic(field.getModifiers())) {
            field.setAccessible(true);
        }
        try {
            field.set(target, value);
        } catch (IllegalAccessException iae) {
            String msg = I18n.loc("POJOSE-7001: Error {0} injecting field {1}.", iae, field);
            logger.severe(msg);
            throw new IllegalArgumentException(msg, iae);
        }
    }
}
