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
 * @(#)AspectSELoggingEndpoint.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.aspect.endpoint;

import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.logging.Handler;

import com.sun.jbi.engine.aspect.endpoint.handler.AspectConstants;
import javax.jbi.JBIException;
import javax.management.AttributeChangeNotification;
import javax.management.InvalidAttributeValueException;
import javax.management.MBeanException;
import javax.management.NotCompliantMBeanException;
import javax.management.Notification;

import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.crl.mep.ExchangeContext;
import com.sun.jbi.crl.mep.exchange.CRLInOnly;
import com.sun.jbi.crl.mep.exchange.CRLInOut;
import com.sun.jbi.engine.aspect.endpoint.handler.AspectSELoggingEndpointHandler;
import com.sun.jbi.engine.aspect.endpoint.mbean.AspectSEEndpointConfiguration;
import com.sun.jbi.engine.aspect.endpoint.mbean.LoggingAspectSEEndpointConfiguration;
import com.sun.jbi.engine.aspect.endpoint.mbean.LoggingAspectSEEndpointConfigurationMbean;
import java.io.File;
import java.util.logging.FileHandler;
import javax.management.InstanceNotFoundException;
import javax.management.MBeanRegistrationException;

/**
 *
 * Logging Aspect The Logging aspect can be used to control logging output. The
 * logging Level objects are ordered and are specified by ordered integers.
 * Enabling logging at a given level also enables logging at all higher levels.
 *
 * The levels in descending order are:
 *
 * <ul>
 * <li> SEVERE (highest value)
 * <li> WARNING
 * <li> INFO
 * <li>CONFIG
 * <li>FINE
 * <li> FINER
 * <li> FINEST (lowest value)
 * </ul>
 * In addition there is a level OFF that can be used to turn off logging, and a
 * level ALL that can be used to enable logging of all messages.
 *
 * <p>
 * This pattern ensures the Logging policy acts between the client and the
 * service. Therefore, if this aspect is configured, all messages flowing
 * between the client and the service providers flow through the Logging engine
 * core.
 *
 * <p>
 * Composite Apps requiring use of this pattern will have to employ either the
 * <b>Filter Request/Reply</b> or the <b>Filter One-Way</b> exchange pattern.
 *
 *
 * <p>
 * Logging Management Configuration
 * <li>1. Get/Set the Logging Level
 *
 *
 *
 * @author Sujit Biswas
 *
 */
public class AspectSELoggingEndpoint extends AspectSEEndpoint {

        String USERDIR = System.getProperties().getProperty("user.dir");
        String FILESEPERATOR = System.getProperties().getProperty("file.separator");
        private static Logger logger = Logger.getLogger(AspectSELoggingEndpoint.class.getName());

        //Configurable Parameters
        private String logfile = null; // This would go into glassfish logs until set explicitly
        private String loglevel = "INFO";  //Default is Info
        private String rotationPolicy = null; //No Rotation by Default

	/**
	 *
	 * @param info
	 */
	public AspectSELoggingEndpoint(EndpointInfo info) {
		super(info);
		setHandler(new AspectSELoggingEndpointHandler(this));
	}

	@Override
	public void init() {
		getHandler().parse();
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see com.sun.jbi.engine.aspect.endpoint.AspectSEEndpointI#handleFilterInvoke(com.sun.jbi.crl.mep.exchange.CRLInOnly,
	 *      com.sun.jbi.crl.mep.ExchangeContext)
	 */
	@Override
	public void handleFilterInvoke(CRLInOnly inOnly, ExchangeContext ctx) throws JBIException
        {
	    logger.log(Level.FINEST, "IN message (InOnly MEP) :: " + inOnly.getInMessage().toString());
		super.handleFilterInvoke(inOnly, ctx);
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see com.sun.jbi.engine.aspect.endpoint.AspectSEEndpointI#handleFilterInvoke(com.sun.jbi.crl.mep.exchange.CRLInOut,
	 *      com.sun.jbi.crl.mep.ExchangeContext)
	 */
	@Override
	public void handleFilterInvoke(CRLInOut inOut, ExchangeContext ctx)
			throws JBIException {
		logger.log(Level.FINEST, "IN message (InOut MEP) :: " + inOut.toString());
		super.handleFilterInvoke(inOut, ctx);
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see com.sun.jbi.engine.aspect.endpoint.AspectSEEndpointI#handleFilterResponse(com.sun.jbi.crl.mep.exchange.CRLInOut,
	 *      com.sun.jbi.crl.mep.exchange.CRLInOut)
	 */
	@Override
	public void handleFilterResponse(CRLInOut inOut, CRLInOut originalInOut,
			ExchangeContext ctx) throws JBIException {
		logger.log(Level.INFO, "OUT message " + inOut.toString());
		super.handleFilterResponse(inOut, originalInOut, ctx);
	}

	public void shutdown() throws InstanceNotFoundException, MBeanRegistrationException
        {
                Handler[] fileHandlers = this.logger.getHandlers();
                this.logger.log(Level.FINE, "Closing [" + fileHandlers.length + "] Open File Loggers");
                for (int i=0; i < fileHandlers.length; i++)
                {
                    this.logger.removeHandler(fileHandlers[i]);
                    fileHandlers[i].close();
                }
		super.shutdown();
	}

	/**
	 *
	 */
	@Override
	public void registerMbean() {
		try {
			AspectSEEndpointConfiguration mbean = new LoggingAspectSEEndpointConfiguration(this.loglevel, this.logfile, this.rotationPolicy);
			mbean.addNotificationListener(this, null, null);
			super.registerMbean(mbean, "Logging");
		} catch (NotCompliantMBeanException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	public void handleNotification(Notification notification, Object obj)
        {
		log().log(Level.INFO, "AspectSELoggingEndpoint.Handling_notification");
		String attributeName = null;
		AttributeChangeNotification attributeNotification = null;
		String attributeValue = null;
		if (notification instanceof AttributeChangeNotification)
                {
			attributeNotification = (AttributeChangeNotification) notification;

			// Check if configuration change is for Aspect SE (Logging) component
			if (attributeNotification.getSource() instanceof LoggingAspectSEEndpointConfigurationMbean)
                        {
				attributeName = attributeNotification.getAttributeName();
				attributeValue = (String) attributeNotification.getNewValue();

				if (true == attributeName.equals(AspectConstants.LOG_TAG_VERBOSITY))
                                {
					try
                                        {
						setVerbosity(attributeValue);
					} catch (InvalidAttributeValueException e) {
						e.printStackTrace();
					} catch (MBeanException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
				}

				if (true == attributeName.equals(AspectConstants.LOG_TAG_FILE))
                                {
					try
                                        {
						setLogfile(attributeValue);
					} catch (InvalidAttributeValueException e) {
						e.printStackTrace();
					} catch (MBeanException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
				}

				if (true == attributeName.equals(AspectConstants.LOG_TAG_ROTATIONPOLICY))
                                {
					try
                                        {
						setRotationpolicy(attributeValue);
					} catch (InvalidAttributeValueException e) {
						e.printStackTrace();
					} catch (MBeanException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
				}

				// persist the changes to the corresponding config file
				save();
			}
		}
	}

	/**
	 *
	 * @return
	 * @throws javax.management.InvalidAttributeValueException
	 * @throws javax.management.MBeanException
	 */
	public String getVerbosity() throws InvalidAttributeValueException,MBeanException
        {
		return this.loglevel.toString();
	}

	/**
	 *
	 * @param levelStr
	 * @throws javax.management.InvalidAttributeValueException
	 * @throws javax.management.MBeanException
	 */
	public void setVerbosity(String levelStr) throws InvalidAttributeValueException, MBeanException
        {
            try
            {
                Level templevel = Level.parse(levelStr);
                if(templevel != null)
                {
                    logger.log(Level.ALL, "Logging Level Being Set to : " + levelStr);
                    this.logger.setLevel(templevel);
                    this.loglevel = levelStr;
                }
            }
            catch(IllegalArgumentException ex)
            {
                logger.log(Level.SEVERE, "Unable to set Logging Level [" + levelStr + "]. Set the valid Log Levels only : " + ex);
                logger.log(Level.INFO, "Valid Logging levels are : ALL, CONFIG, FINE, FINER, FINEST, INFO, OFF, SEVERE, WARNING");
            }
	}

	/**
	 *
	 * @return
	 * @throws javax.management.InvalidAttributeValueException
	 * @throws javax.management.MBeanException
	 */
	public String getLogfile() throws InvalidAttributeValueException,MBeanException
        {
		return this.logfile;
	}

	public void setLogfile(String logFileAbsPath) throws InvalidAttributeValueException, MBeanException
        {
            boolean status = validateLogFilePath(logFileAbsPath);
            if (status)
            {
                try
                {
                    FileHandler filehandler = null;

                    if (this.rotationPolicy == null)
                    {
                        filehandler = new FileHandler(this.logfile, true);
                    }
                    else
                    {
                        filehandler = new FileHandler(this.logfile + "%g.log", Integer.parseInt(this.rotationPolicy), 10, true);
                    }

                    logger.addHandler(filehandler);
                    logger.log(Level.INFO, "Created File Handler for File : " + this.logfile);
                }
                catch (IOException ex)
                {
                    logger.log(Level.SEVERE, "Exception in Creating FileHandler for File :" + this.logfile);
                }
            }
	}

	/**
	 *
	 * @return
	 * @throws javax.management.InvalidAttributeValueException
	 * @throws javax.management.MBeanException
	 */
	public String getRotationpolicy() throws InvalidAttributeValueException,MBeanException
        {
		return this.rotationPolicy;
	}

	/**
	 *
	 * @param rotationPolicy
	 * @throws javax.management.InvalidAttributeValueException
	 * @throws javax.management.MBeanException
	 */
	public void setRotationpolicy(String rotationPolicy) throws InvalidAttributeValueException, MBeanException
        {
            if (rotationPolicy != null)
            {
                //Check if the input String is the number
                try
                {
                    Integer.parseInt(rotationPolicy);
                    this.rotationPolicy = rotationPolicy;
                    logger.log(Level.INFO, "Setting Log Rotation Policy to :: " + this.rotationPolicy + " bytes");
                }
                catch(NumberFormatException ex)
                {
                    logger.log(Level.SEVERE, "Cannot Set String value to Roration Policy. Set the policy with log file size (in bytes) to be rotated");
                }
            }
	}

        private boolean validateLogFilePath(String logFilePath)
        {
            boolean filePathOK = true;

            if (logFilePath.indexOf(this.FILESEPERATOR) == -1)
            {
                try
                {
                    String sysLogDir = USERDIR.substring(0,USERDIR.lastIndexOf(FILESEPERATOR) + 1) + "logs";
                    File f = new java.io.File(sysLogDir + this.FILESEPERATOR + logFilePath);
                    Boolean fileCreateOK = f.createNewFile();
                    this.logfile = f.getAbsolutePath();
                    if (fileCreateOK) logger.log(Level.INFO, "Created New Log File [" + logFilePath + "] under " + sysLogDir);
                }
                catch (IOException ex)
                {
                    filePathOK = false;
                    logger.log(Level.SEVERE, "Unable to create the log file. Pls check the path :: " + logFilePath);
                }
            }
            else
            {
                try
                {
                    File f = new File(logFilePath);
                    boolean fileCreateOK = f.createNewFile();
                    this.logfile = f.getAbsolutePath();
                    if (fileCreateOK) logger.log(Level.INFO, "Created New Log File [" + logFilePath + "]");
                }
                catch (IOException ex)
                {
                    filePathOK = false;
                    logger.log(Level.SEVERE, "Unable to create the log file. Pls check the path :: " + logFilePath);
                }
            }

            return filePathOK;
	}
}
