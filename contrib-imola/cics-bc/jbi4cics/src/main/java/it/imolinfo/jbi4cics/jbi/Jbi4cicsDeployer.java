/*
 *  Copyright (c) 2005, 2006 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 */


package it.imolinfo.jbi4cics.jbi;

import it.imolinfo.jbi4cics.Logger;
import it.imolinfo.jbi4cics.LoggerFactory;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import javax.jbi.management.DeploymentException;
import org.apache.servicemix.common.BaseComponent;
import org.apache.servicemix.common.Endpoint;
import org.apache.servicemix.common.ServiceUnit;
import org.apache.servicemix.common.xbean.AbstractXBeanDeployer;

public final class Jbi4cicsDeployer extends AbstractXBeanDeployer {

    /**
     * The task name, used for error messages.
     */
    private static final String TASK = "deploy";

    /**
     * The logger for this class and its instances.
     */
    private static final Logger LOG
            = LoggerFactory.getLogger(Jbi4cicsDeployer.class);

    /**
     * The responsible to translate localized messages.
     */
    private static final Messages MESSAGES
            = Messages.getMessages(Jbi4cicsDeployer.class);

    /**
     * The path of the exploded service unit.
     */
    private String suRootPath;

    /**
     * Initializes this deployer for the specified component.
     *
     * @param  component  the component that will be deployed by this instance.
     */
    public Jbi4cicsDeployer(final BaseComponent component) {
        super(component);
    }

    /**
     * Deploys the given service unit and build a <code>ServiceUnit</code>
     * object that contains endpoints.
     *
     * @param   suName      the name of the service unit.
     * @param   suRootPath  the path of the exploded service unit.
     * @return  a service unit containing endpoints.
     * @throws  DeploymentException  if an error occurs.
     */
    @Override
    public ServiceUnit deploy(final String suName, final String suRootPath)
            throws DeploymentException {                        // Overridden
        this.suRootPath = suRootPath;
        return super.deploy(suName, suRootPath);
    }

    /**
     * Validates the specified endpoint.
     *
     * @param   endpoint             the endpoint to validate.
     * @throws  DeploymentException  if <code>endpoint</code> is not an instance
     *                               of {@link Jbi4cicsEndpoint} class or can't
     *                               be validated.
     */
    @Override
    protected void validate(final Endpoint endpoint)            // Overridden
            throws DeploymentException {
        Jbi4cicsEndpoint cicsEndpoint;
        String fileName;

        if (!(endpoint instanceof Jbi4cicsEndpoint)) {
            throw failure(TASK,
                    MESSAGES.getString("CIC001010_No_jbi4cics_endpoint"), null);
        }
        cicsEndpoint = (Jbi4cicsEndpoint) endpoint;

        // The copy Cobol used for input and, optionally, for output
        fileName = cicsEndpoint.getCopyCobolFileName();
        cicsEndpoint.setCopyCobol(
                readFile(suRootPath + File.separator + fileName));

        // If found, the output copy Cobol differs from input copy Cobol
        fileName = cicsEndpoint.getOutputCopyCobolFileName();
        if ((fileName != null) && (fileName.length() > 0)) {
            cicsEndpoint.setOutputCopyCobol(
                    readFile(suRootPath + File.separator + fileName));
        }

        try {
            cicsEndpoint.registerService();
        } catch (Exception e) {
            Object[] args = new Object[] { e.getLocalizedMessage() };

            LOG.error("CIC001012_Error_registering_endpoint", args, e);
            throw failure(TASK, MESSAGES.getString(
                    "CIC001012_Error_registering_endpoint", args), e);
        }
    }

    /**
     * Gets the specified file content as a string.
     *
     * @param   fileName             the name of the file to read. Must be not
     *                               <code>null</code>.
     * @return  a string representing the file content. The returned string is
     *          never <code>null</code>.
     * @throws  DeploymentException  if the file does not exist, is a directory
     *                               rather than a regular file, for some other
     *                               reason cannot be opened for reading or an
     *                               I/O error occurs.
     */
    private String readFile(final String fileName) throws DeploymentException {
        File file = new File(fileName);
        char[] buf = new char[(int) file.length()];
        FileReader reader = null;

        try {
            int offset = 0;

            reader = new FileReader(file);
            do {
                int count = reader.read(buf, offset, buf.length - offset);

                offset += count;
            } while (offset < buf.length);
        } catch (IOException e) {
            throw createFailure(fileName, e);
        } finally {
            if (reader != null) {
                try {
                    reader.close();
                } catch (IOException e) {
                    throw createFailure(fileName, e);
                }
            }
        }
        return new String(buf);
    }

    /**
     * Creates a new <code>DeploymentException</code> caused by the specified
     * <code>IOException</code> obtained while accessing the file named
     * <code>fileName</code>, logging also the <code>IOException</code> itself.
     *
     * @param   fileName             the name of the file to be read. Must be
     *                               not <code>null</code>.
     * @param   ioe                  the exception received while accessing the
     *                               file named <code>fileName</code>. Must be
     *                               not <code>null</code>.
     * @return  the newly created failure, containing error details regarding
     *          <code>fileName</code> and <code>ioe</code>.
     */
    private DeploymentException createFailure(final String fileName, 
                                              final IOException ioe) {
        Object[] args = new Object[] { fileName, ioe.getLocalizedMessage() };
        String errorMsg = MESSAGES.getString(
                "CIC001011_Error_reading_copy_file", args);

        LOG.error("CIC001011_Error_reading_copy_file", args, ioe);
        return failure(TASK, errorMsg, ioe);
    }
}
