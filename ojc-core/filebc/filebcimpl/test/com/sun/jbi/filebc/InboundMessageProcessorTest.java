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
 * @(#)InboundMessageProcessorTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.filebc;

import junit.framework.*;
import com.sun.jbi.eManager.provider.EndpointStatus;
import com.sun.jbi.filebc.extensions.FileOperation;
import com.sun.jbi.filebc.extensions.FileAddress;
import com.sun.jbi.filebc.extensions.FileInput;
import com.sun.jbi.filebc.extensions.FileOutput;
import com.sun.jbi.filebc.extensions.FileMessage;
import com.sun.jbi.common.qos.messaging.MessagingChannel;

import com.sun.jbi.filebc.util.FileUtil;
import com.sun.jbi.filebc.util.InputFilenameFilter;
import com.sun.jbi.filebc.util.InputDirFilter;

import java.io.File;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.locks.ReentrantLock;
import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessageExchangeFactory;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;
import java.io.FileOutputStream;
import java.io.FilenameFilter;
import java.util.UUID;
import java.util.concurrent.LinkedBlockingQueue;
import javax.wsdl.Binding;
import javax.wsdl.Definition;
import javax.wsdl.Port;
import javax.wsdl.PortType;
import javax.wsdl.Service;
import org.jmock.*;

/**
 *
 * @author sweng
 */
public class InboundMessageProcessorTest extends org.jmock.cglib.MockObjectTestCase {

    static final QName THE_OPERATION = new QName("myoperation");
    private static final String strEPName = "myEndpointName";
    private static final String strServiceName = "myServiceName";
    static final QName THE_SERVICE = new QName(strServiceName);
    static final String THE_ENDPOINT = strEPName;
    static final int THE_TYPE = 0;
    InboundMessageProcessor instance = null;
    IBFileWorker worker = null;
    Map<QName, Service> services = null;
    Mock deliveryChannel = null;
    Mock componentContext = null;
    Mock endpoint = null;
    Mock endpointStatus = null;
    Mock serviceEndpoint = null;
    Mock msgExchange = null;
    Mock msgExchangeFactory = null;
    Mock normalizer = null;
    Mock normalizedMsg = null;
    Mock wsdlDefinition = null;
    Mock service = null;
    Mock port = null;
    Mock portType = null;
    Mock binding = null;
    Map operations = new HashMap();
    Map operationMeps = new HashMap();
    File mWorkAreaBaseDir;
    int maxCC = 5;

    public InboundMessageProcessorTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        componentContext = mock(ComponentContext.class);
        deliveryChannel = mock(MessagingChannel.class);
        endpoint = mock(Endpoint.class);
        endpointStatus = mock(EndpointStatus.class);
        serviceEndpoint = mock(ServiceEndpoint.class);
        msgExchangeFactory = mock(MessageExchangeFactory.class);
        msgExchange = mock(MessageExchange.class);
        normalizer = mock(FileNormalizer.class);
        normalizedMsg = mock(NormalizedMessage.class);
        wsdlDefinition = mock(Definition.class);
        service = mock(Service.class);
        port = mock(Port.class);
        portType = mock(PortType.class);
        binding = mock(Binding.class);

        services = new HashMap<QName, Service>();
        services.put(THE_SERVICE, (Service) service.proxy());

        String epUUID = "IB".concat(UUID.nameUUIDFromBytes(strServiceName.concat(strEPName).getBytes()).toString());
        endpoint.expects(atLeastOnce()).method("getEPUUID").will(returnValue(epUUID));
        endpoint.expects(atLeastOnce()).method("getServiceName").will(returnValue(THE_SERVICE));
        endpoint.expects(atLeastOnce()).method("getEndpointName").will(returnValue(THE_ENDPOINT));
        endpoint.expects(atLeastOnce()).method("getDefinition").will(returnValue(wsdlDefinition.proxy()));
        wsdlDefinition.expects(atLeastOnce()).method("getServices").will(returnValue((Map<QName, Service>) services));
        service.expects(atLeastOnce()).method("getPort").will(returnValue((Port) port.proxy()));
        port.expects(atLeastOnce()).method("getBinding").will(returnValue((Binding) binding.proxy()));
        binding.expects(atLeastOnce()).method("getPortType").will(returnValue((PortType) portType.proxy()));
        portType.expects(atLeastOnce()).method("getQName").will(returnValue(QName.valueOf("portType1")));

        deliveryChannel.expects(atLeastOnce()).method("createExchangeFactory").will(returnValue(msgExchangeFactory.proxy()));

        FileComponentContext.getInstance().setBindingChannel((MessagingChannel) deliveryChannel.proxy());

        //String key = strServiceName + strEPName;
        String baseWorkDir = "test/com/sun/jbi/filebc/input/" + strServiceName + "/" + strEPName;
        mWorkAreaBaseDir = new File(baseWorkDir);
        mWorkAreaBaseDir.mkdirs();
        String lockFilePath = baseWorkDir + File.separator + "filebc.lck";

        File lockFile = new File(lockFilePath);
        lockFile.createNewFile();
        FileOutputStream fos = new FileOutputStream(lockFile);
        LockRegistry.register(epUUID, new Lock((fos != null ? fos.getChannel() : null), new ReentrantLock(), lockFilePath));

        instance = new InboundMessageProcessor((ComponentContext) componentContext.proxy(),
                (DeliveryChannel) deliveryChannel.proxy(),
                (Endpoint) endpoint.proxy(),
                THE_OPERATION,5);
    }

    protected void tearDown() throws Exception {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(InboundMessageProcessorTest.class);

        return suite;
    }

    /**
     * Test of run method, of class com.sun.jbi.filebc.InboundMessageProcessor 
     * for the scenario where the message exchange pattern is not valid
     */
    public void testRunInvalidMEP() {
        System.out.println("Testing run() for the scenario where the message exchange pattern is not valid");

        FileOperation fileoperation = new FileOperation();
        operations.put(THE_OPERATION, fileoperation);

        FileInput fileInput = new FileInput();
        fileInput.setFileMessage(new FileMessage());
        fileoperation.setFileOperationInput(fileInput);

        FileAddress addr = new FileAddress();
        addr.setFileDirectory("test/com/sun/jbi/filebc/input");
        endpoint.expects(atMostOnce()).method("getEndpointName").will(returnValue(THE_ENDPOINT));
        endpoint.expects(atLeastOnce()).method("getFileOperations").will(returnValue(operations));
        endpoint.expects(atLeastOnce()).method("getOperationMsgExchangePattern").will(returnValue(operationMeps));

        // 1. testing the case where message exchange pattern is not found (null)
        operationMeps.remove(THE_OPERATION);
        operationMeps.put("dummyOperation", "inonly");

        instance.stopReceiving();
        // for junit testing - avoid multiple inbound worker threads
        instance.run();

        // 2. testing the case where message exchange status is invalid (not inonly or inout)
        operationMeps.put(THE_OPERATION, "outin");
        instance.run();
    }

    /**
     * Test of execute method, of class com.sun.jbi.filebc.InboundMessageProcessor
     * for the scenario where basic file extensibility element attribute validation fails.
     */
    public void testValidateInboundMessageProperties() {
        System.out.println("Testing basic runtime wsdl validation");

        String mep = "outin";
        FileOperation operation = new FileOperation();
        FileInput fileInput = new FileInput();
        FileOutput fileOutput = new FileOutput();
        FileMessage fileMessage = new FileMessage();

        // testing an invalid mep
        try {
            instance.validateInboundMessageExchangeProperties(operation, mep);
            fail("Failed to test validateInboundMessageProperties - an exception should be raised due to invalid message exchange pattern: " + mep);
        } catch (Exception e) {
            System.out.println("Successfully tested validateInboundMessageProperties with an invalid message exchange pattern: " + mep);
        }

        // testing another invalid mep
        mep = "unsupported";
        try {
            instance.validateInboundMessageExchangeProperties(operation, mep);
            fail("Failed to test validateInboundMessageProperties - an exception should be raised due to invalid message exchange pattern: " + mep);
        } catch (Exception e) {
            System.out.println("Successfully tested validateInboundMessageProperties with an invalid message exchange pattern: " + mep);
        }

        mep = "inonly";
        // testing null input properties
        try {
            instance.validateInboundMessageExchangeProperties(operation, mep);
            fail("Failed to test validateInboundMessageProperties - an exception should be raised due to null File Input properties.");
        } catch (Exception e) {
            System.out.println("Successfully tested validateInboundMessageProperties with null File Input properties.");
        }

        // testing null File read properties
        operation.setFileOperationInput(fileInput);
        try {
            instance.validateInboundMessageExchangeProperties(operation, mep);
            fail("Failed to test validateInboundMessageProperties - an exception should be raised due to null File Read properties.");
        } catch (Exception e) {
            System.out.println("Successfully tested validateInboundMessageProperties with null File Read properties.");
        }

        // testing missing required File Read attributes
        operation.setFileOperationInput(fileInput);
        fileInput.setFileMessage(fileMessage);
        try {
            instance.validateInboundMessageExchangeProperties(operation, mep);
            fail("Failed to test validateInboundMessageProperties - an exception should be raised due to missing required File Read attribute: pollingMillis.");
        } catch (Exception e) {
            System.out.println("Successfully tested validateInboundMessageProperties: missing required File Read attribute: pollingMillis.");
        }

        operation.setFileOperationInput(fileInput);
        fileInput.setFileMessage(fileMessage);
        fileMessage.setPollingInterval(new Long(5000));
        try {
            instance.validateInboundMessageExchangeProperties(operation, mep);
            fail("Failed to test validateInboundMessageProperties - an exception should be raised due to missing required File Read attribute: fileName.");
        } catch (Exception e) {
            System.out.println("Successfully tested validateInboundMessageProperties: missing required File Read attribute: fileName.");
        }

        // testing invalid File Read attribute value
        fileMessage.setFileType("stream");
        try {
            instance.validateInboundMessageExchangeProperties(operation, mep);
            fail("Failed to test validateInboundMessageProperties - an exception should be raised due to invalid File Read attribute value: fileType is stream.");
        } catch (Exception e) {
            System.out.println("Successfully tested validateInboundMessageProperties: invalid File Read attribute value: fileType is stream.");
        }

        // testing with in-out mep
        // testing with null File output properties
        mep = "inout";
        operation.setFileOperationOutput(fileOutput);
        try {
            instance.validateInboundMessageExchangeProperties(operation, mep);
            fail("Failed to test validateInboundMessageProperties - an exception should be raised due to null File Output properties.");
        } catch (Exception e) {
            System.out.println("Successfully tested validateInboundMessageProperties with null File Output properties.");
        }

        // testing missing required File write attributes
        try {
            instance.validateInboundMessageExchangeProperties(operation, mep);
            fail("Failed to test validateInboundMessageProperties - an exception should be raised due to undefined fileName attribute when fileNameIsPattern is false.");
        } catch (Exception e) {
            System.out.println("Successfully tested validateInboundMessageProperties when fileName is not defined and fileNameIsPattern is false.");
        }

        fileMessage.setFileNameIsPattern(Boolean.TRUE);
        try {
            instance.validateInboundMessageExchangeProperties(operation, mep);
            fail("Failed to test validateInboundMessageProperties - an exception should be raised due to undefined filePrefix and fileExtension attributes when fileNameIsPattern is true.");
        } catch (Exception e) {
            System.out.println("Successfully tested validateInboundMessageProperties when both filePrefix and fileExtension are not defined and fileNameIsPattern is true.");
        }

        // testing invalid File Write attribute value
        fileMessage.setFileType("undefined");
        try {
            instance.validateInboundMessageExchangeProperties(operation, mep);
            fail("Failed to test validateInboundMessageProperties - an exception should be raised due to invalid File Write attribute value: fileType is undefined.");
        } catch (Exception e) {
            System.out.println("Successfully tested validateInboundMessageProperties: invalid File Write attribute value: fileType is undefined.");
        }

        System.out.println("Successfully tested basic runtime wsdl validation");
    }

    /**
     * Test of execute method, of class com.sun.jbi.filebc.InboundMessageProcessor
     * for the scenario where the message exchange type is inonly.
     */
    public void testExecuteInOnlyExchangeCase1() {
        System.out.println("Testing in-only message exchange case 1...");

        FileInput fileInput = new FileInput();
        FileMessage fileMessage = new FileMessage();
        FileAddress fileAddress = new FileAddress();
        msgExchange = mock(InOnly.class);

        fileAddress.setFileDirectory("test/com/sun/jbi/filebc/input");

        fileAddress.setLockName(Lock.DEFAULT_INBOUND_LOCKFILE_NAME);
        fileAddress.setWorkArea(Lock.DEFAULT_INBOUND_TMPDIR_NAME);
        fileAddress.setSeqName(Lock.DEFAULT_SEQFILE_NAME);

        // after cluster support changes
        // processed files are no longer marked just as <original_input_file_name>_processed and still stay at the same directory
        // instead, it is either deleted (if archive = false) or UUID tagged and _processed suffixed as <original_input_file_name><UUID>_processed and
        // moved to archive directory (if archive = true)
        // keep it here - in case in some test context inheritated from past - this file can be removed
        // before test starts
        String inputFile = fileAddress.getFileDirectory().concat("/InputInOnly.txt");
        String testFile = fileAddress.getFileDirectory().concat("/TestInputInOnly.txt");

        // endpoint specific archive, tmp and errDir
        String archiveDir = mWorkAreaBaseDir.getAbsolutePath().concat("/" + FileMessage.FILE_ARCHIVE_DIR_DEFAULT);
        String tmpDir = mWorkAreaBaseDir.getAbsolutePath().concat("/" + Lock.DEFAULT_INBOUND_TMPDIR_NAME);
        String errDir = mWorkAreaBaseDir.getAbsolutePath() + File.separator + FileUtil.DEFAULT_ERRORS_DIR_NAME;

        // after cluster support changes, the error marked files are kept in working area - default location is <input_dir>/filebc-in-processing
        // the file name looks like: <original_input_file><UUID>_error
        // create the lock for inbound
        cleanup(new String[]{archiveDir, tmpDir, errDir}, "Failed to create input file to test execute for InOnly message exchanges: ");
        validateAndPrepareTestFiles(inputFile, testFile, "Failed to create input file to test execute for InOnly message exchanges: ");

        // Testing the following scenario
        // 1. no file name match pattern
        // 2. successful "normalization"
        // 3. successful msgexchange.send

        fileMessage.setFileName("TestInputInOnly.txt");
        fileMessage.setPollingInterval(new Long(5000));
        fileMessage.setFileNameIsRegex(Boolean.FALSE);
        fileInput.setFileMessage(fileMessage);

        deliveryChannel.expects(atLeastOnce()).method("createExchangeFactory").will(returnValue(msgExchangeFactory.proxy()));
        msgExchangeFactory.expects(atLeastOnce()).method("createInOnlyExchange").will(returnValue((InOnly) msgExchange.proxy()));
        componentContext.expects(atLeastOnce()).method("getEndpoint").with(eq(THE_SERVICE), eq(THE_ENDPOINT)).will(returnValue((ServiceEndpoint) serviceEndpoint.proxy()));
        msgExchange.expects(atLeastOnce()).method("getExchangeId");
        normalizer.expects(atLeastOnce()).method("normalize").withAnyArguments().will(returnValue((NormalizedMessage) normalizedMsg.proxy()));
        msgExchange.expects(once()).method("setEndpoint").with(isA(ServiceEndpoint.class));
        msgExchange.expects(once()).method("setOperation").with(eq(THE_OPERATION));
        msgExchange.expects(once()).method("setMessage").with(eq(normalizedMsg.proxy()), eq("in"));

        deliveryChannel.expects(once()).method("send").with(eq(msgExchange.proxy()));
        endpoint.expects(atLeastOnce()).method("getServiceName").will(returnValue(THE_SERVICE));
        endpoint.expects(atLeastOnce()).method("getEndpointName").will(returnValue(THE_ENDPOINT));
        endpoint.expects(atLeastOnce()).method("getEndpointType").will(returnValue(THE_TYPE));
        endpoint.expects(once()).method("getEndpointStatus").will(returnValue(endpointStatus.proxy()));
        endpointStatus.expects(once()).method("incrementSentRequests");

        // QOS: Re-delivery
        msgExchange.expects(atLeastOnce()).method("getProperty").with(eq(FileComponentContext.CRMP_GROUP_ID)).will(returnValue(ANYTHING));
        msgExchange.expects(atLeastOnce()).method("getProperty").with(eq(FileComponentContext.CRMP_MESSAGE_ID)).will(returnValue(ANYTHING));
        deliveryChannel.expects(once()).method("getServiceQuality");

        // NM Properties related expectations
        normalizedMsg.expects(atLeastOnce()).method("getProperty");
        normalizedMsg.expects(atLeastOnce()).method("setProperty");

        // set no throttling
        endpoint.expects(atLeastOnce()).method("getMaxConcurrencyLimit").will(returnValue(-1));

        endpoint.expects(atLeastOnce()).method("getServiceUnitID");

        try {
            AtomicInteger maxFilesPerPoll = new AtomicInteger(20);
            InputDirFilter dirFilter = /*new InputDirFilter("")*/ null;
            InputFilenameFilter fileFilter = /*new InputFilenameFilter(fileInput.getFileMessage().getFileName(), "", maxFilesPerPoll, false)*/ null;

            instance.addWorker(worker = new IBFileWorker(instance, "inonly", fileMessage,
                    new File(fileAddress.getFileDirectory()), mWorkAreaBaseDir));
            worker.setNormalizer((FileNormalizer) normalizer.proxy());
            instance.setLock(createLock(fileAddress.getFileDirectory(), fileAddress.getLockName()));
            instance.setStopped(true); // let the worker loop once and quit
            instance.setFiles(new LinkedBlockingQueue());
            instance.execute("inonly",
                    fileAddress.getFileDirectory(),
                    "TestInputInOnly.txt",
                    Boolean.FALSE,
                    fileInput,
                    fileAddress,
                    maxFilesPerPoll,
                    fileFilter,
                    dirFilter,maxCC);

            Thread.sleep(5000);
            // since it is a mocked framework - there is no Outbound processor to invoke the replyMessage(...)
            // which actually do the _processed suffixing and moving the input from work area to archive area
            // so it is good enough to assert that the UUID tagged input file is in work area
            assertTrue("Failed to find the input file in tmp dir...", hasFilesWithSuffix(tmpDir, ""));
        } catch (Exception e) {
            e.printStackTrace();
            fail("Failed to test InOnly exchanges due to: " + e.getMessage());
        }

        verifyContract();
        cleanup(new String[]{archiveDir, tmpDir, errDir}, "Failed to create input file to test execute for InOnly message exchanges: ");

        // Testing the following scenario
        // 1. no file name match pattern
        // 2. Unsuccessful "normalization"
        validateAndPrepareTestFiles(inputFile, testFile, "Failed to create input file to test execute for InOnly message exchanges: ");

        normalizer.expects(atLeastOnce()).method("normalize").withAnyArguments().will(throwException(new Exception("Failed to normalize")));

        try {
            AtomicInteger maxFilesPerPoll = new AtomicInteger(20);
            InputDirFilter dirFilter = /*new InputDirFilter("")*/ null;
            InputFilenameFilter fileFilter = /*new InputFilenameFilter(fileInput.getFileMessage().getFileName(), "", maxFilesPerPoll, false)*/ null;

            instance.setWorkers(null);
            instance.addWorker(worker = new IBFileWorker(instance, "inonly", fileMessage,
                    new File(fileAddress.getFileDirectory()), mWorkAreaBaseDir));
            worker.setNormalizer((FileNormalizer) normalizer.proxy());
            instance.setLock(createLock(fileAddress.getFileDirectory(), fileAddress.getLockName()));
            instance.setStopped(true); // let the worker loop once and quit
            instance.execute("inonly",
                    fileAddress.getFileDirectory(),
                    "TestInputInOnly.txt",
                    Boolean.FALSE,
                    fileInput,
                    fileAddress,
                    maxFilesPerPoll,
                    fileFilter,
                    dirFilter,maxCC);
            Thread.sleep(5000);
            // now there are worker threads spin off by execute(), the exception "Failed to normalize" is thrown
            // in the run method of IBFileWorker and is captured and logged and
            // the offending message is marked as <original_input_file_name><UUID>_error and will stay in the
            // work area - tmp dir
            // the next best assertion for this scenario is to assert that the _error file is there
            assertTrue("Failed to find the _error suffixed input file in error dir...", hasFilesWithSuffix(errDir, FileUtil.getErrorFileSuffix()));
            //fail("Failed to test in-only message exchange when an expected exception should be caught");
        } catch (Exception e) {
//            System.out.println("Caught the expected exception!!");
            fail("Failed to test execute for InOnly message exchanges with Exception 'Failed to normalize': " + e.getMessage());
        }
        verifyContract();
        System.out.println("Successfully tested in-only message exchange case 1");
    }

    /**
     * Test of execute method, of class com.sun.jbi.filebc.InboundMessageProcessor
     * for the scenario where the message exchange type is inonly.
     */
    public void testExecuteInOnlyExchangeCase2() {
        System.out.println("Testing in-only message exchange case 2...");

        FileInput fileInput = new FileInput();
        FileMessage fileMessage = new FileMessage();
        FileAddress fileAddress = new FileAddress();
        msgExchange = mock(InOnly.class);

        fileAddress.setFileDirectory("test/com/sun/jbi/filebc/input");

        fileAddress.setLockName(Lock.DEFAULT_INBOUND_LOCKFILE_NAME);
        fileAddress.setWorkArea(Lock.DEFAULT_INBOUND_TMPDIR_NAME);
        fileAddress.setSeqName(Lock.DEFAULT_SEQFILE_NAME);

        // after cluster support changes
        // processed files are no longer marked just as <original_input_file_name>_processed and still stay at the same directory
        // instead, it is either deleted (if archive = false) or UUID tagged and _processed suffixed as <original_input_file_name><UUID>_processed and
        // moved to archive directory (if archive = true)
        String inputFile = fileAddress.getFileDirectory().concat("/InputInOnly.txt");
        String testFile = fileAddress.getFileDirectory().concat("/I8n.dat");

        String archiveDir = mWorkAreaBaseDir.getAbsolutePath().concat("/" + FileMessage.FILE_ARCHIVE_DIR_DEFAULT);
        String tmpDir = mWorkAreaBaseDir.getAbsolutePath().concat("/" + Lock.DEFAULT_INBOUND_TMPDIR_NAME);
        String errDir = mWorkAreaBaseDir.getAbsolutePath() + File.separator + FileUtil.DEFAULT_ERRORS_DIR_NAME;

        cleanup(new String[]{archiveDir, tmpDir, errDir}, "Failed to clean up archive directory and filebc-in-processing directory to test execute for InOnly message exchanges: ");
        validateAndPrepareTestFiles(inputFile, testFile, "Failed to create input file to test execute for InOnly message exchanges: ");

        // Testing the following scenario
        // 1. file name is pattern
        // 2. successful "normalization"
        // 3. successful msgexchange.send
        fileMessage.setFileName("Input%u.txt");
        fileMessage.setPollingInterval(new Long(5000));
        fileInput.setFileMessage(fileMessage);

        deliveryChannel.expects(atLeastOnce()).method("createExchangeFactory").will(returnValue(msgExchangeFactory.proxy()));
        msgExchangeFactory.expects(atLeastOnce()).method("createInOnlyExchange").will(returnValue((InOnly) msgExchange.proxy()));
        componentContext.expects(atLeastOnce()).method("getEndpoint").with(eq(THE_SERVICE), eq(THE_ENDPOINT)).will(returnValue((ServiceEndpoint) serviceEndpoint.proxy()));
        msgExchange.expects(atLeastOnce()).method("getExchangeId");
        normalizer.expects(atLeastOnce()).method("normalize").withAnyArguments().will(returnValue((NormalizedMessage) normalizedMsg.proxy()));
        msgExchange.expects(once()).method("setEndpoint").with(isA(ServiceEndpoint.class));
        msgExchange.expects(once()).method("setOperation").with(eq(THE_OPERATION));
        msgExchange.expects(once()).method("setMessage").with(eq(normalizedMsg.proxy()), eq("in"));

        deliveryChannel.expects(once()).method("send").with(eq(msgExchange.proxy()));
        //endpoint.expects(atLeastOnce()).method("getServiceName").will(returnValue(THE_SERVICE));
        endpoint.expects(atLeastOnce()).method("getEndpointName").will(returnValue(THE_ENDPOINT));
        endpoint.expects(once()).method("getEndpointStatus").will(returnValue(endpointStatus.proxy()));
        endpoint.expects(atLeastOnce()).method("getEndpointType").will(returnValue(THE_TYPE));
        endpointStatus.expects(once()).method("incrementSentRequests");

        // QOS: Re-delivery
        msgExchange.expects(atLeastOnce()).method("getProperty").with(eq(FileComponentContext.CRMP_GROUP_ID)).will(returnValue(ANYTHING));
        msgExchange.expects(atLeastOnce()).method("getProperty").with(eq(FileComponentContext.CRMP_MESSAGE_ID)).will(returnValue(ANYTHING));
        deliveryChannel.expects(once()).method("getServiceQuality");

        // NM Properties related expectations
        normalizedMsg.expects(atLeastOnce()).method("getProperty");
        normalizedMsg.expects(atLeastOnce()).method("setProperty");

        // set no throttling
        endpoint.expects(atLeastOnce()).method("getMaxConcurrencyLimit").will(returnValue(-1));

        endpoint.expects(atLeastOnce()).method("getServiceUnitID");

        try {
            AtomicInteger maxFilesPerPoll = new AtomicInteger(20);
            InputDirFilter dirFilter = /*new InputDirFilter("")*/ null;
            InputFilenameFilter fileFilter = new InputFilenameFilter(fileInput.getFileMessage().getFileName(), "", maxFilesPerPoll, false);

            instance.addWorker(worker = new IBFileWorker(instance, "inonly", fileMessage,
                    new File(fileAddress.getFileDirectory()), mWorkAreaBaseDir));
            worker.setNormalizer((FileNormalizer) normalizer.proxy());
            instance.setLock(createLock(fileAddress.getFileDirectory(), fileAddress.getLockName()));
            instance.setStopped(true); // let the worker loop once and quit
            instance.setFiles(new LinkedBlockingQueue());
            fileFilter.setFilterExpression("I%dn.dat");
            instance.execute("inonly",
                    fileAddress.getFileDirectory(),
                    "I%dn.dat",
                    Boolean.TRUE,
                    fileInput,
                    fileAddress,
                    maxFilesPerPoll,
                    fileFilter,
                    dirFilter,maxCC);
            Thread.sleep(5000);
            assertTrue("Failed to find the input file in tmp dir...", hasFilesWithSuffix(tmpDir, ""));
        } catch (Exception e) {
            fail("Failed to test InOnly exchanges due to: " + e.getMessage());
        }

        verifyContract();

        // Testing the following scenario
        // 1. file name is pattern
        // 2. Unsuccessful "normalization"
        cleanup(new String[]{archiveDir, tmpDir, errDir}, "Failed to clean up archive directory and filebc-in-processing directory to test execute for InOnly message exchanges: ");
        validateAndPrepareTestFiles(inputFile, testFile, "Failed to create input file to test execute for InOnly message exchanges: ");

        normalizer.expects(atLeastOnce()).method("normalize").withAnyArguments().will(throwException(new Exception("Failed to normalize")));

        try {
            AtomicInteger maxFilesPerPoll = new AtomicInteger(20);
            InputDirFilter dirFilter = new InputDirFilter("");
            InputFilenameFilter fileFilter = new InputFilenameFilter(fileInput.getFileMessage().getFileName(), "", maxFilesPerPoll, false);

            instance.setWorkers(null);
            instance.addWorker(worker = new IBFileWorker(instance, "inonly", fileMessage,
                    new File(fileAddress.getFileDirectory()), mWorkAreaBaseDir));
            worker.setNormalizer((FileNormalizer) normalizer.proxy());
            instance.setLock(createLock(fileAddress.getFileDirectory(), fileAddress.getLockName()));
            instance.setStopped(true); // let the worker loop once and quit
            fileFilter.setFilterExpression("I%dn.dat");
            instance.execute("inonly",
                    fileAddress.getFileDirectory(),
                    "I%dn.dat",
                    Boolean.TRUE,
                    fileInput,
                    fileAddress,
                    maxFilesPerPoll,
                    fileFilter,
                    dirFilter,maxCC);
            Thread.sleep(5000);
            // now there are worker threads spin off by execute(), the exception "Failed to normalize" is thrown
            // in the run method of IBFileWorker and is captured and logged and
            // the offending message is marked as <original_input_file_name><UUID>_error and will stay in the
            // work area - tmp dir
            // the next best assertion for this scenario is to assert that the _error file is there
            assertTrue("Failed to find the _error suffixed input file in error dir...", hasFilesWithSuffix(errDir, FileUtil.getErrorFileSuffix()));
            //fail("Failed to test in-only message exchange when an expected exception should be caught");
        } catch (Exception e) {
            fail("Failed to test execute for InOnly message exchanges with Exception 'Failed to normalize': " + e.getMessage());
            //System.out.println("Caught the expected exception!!");
        }
        verifyContract();
        System.out.println("Successfully tested in-only message exchange case 2");
    }

    /**
     * Test of execute method, of class com.sun.jbi.filebc.InboundMessageProcessor
     * for the scenario where the message exchange type is inonly.
     */
    public void testExecuteInOnlyExchangeCase3() {
        System.out.println("Testing in-only message exchange case 3...");

        FileInput fileInput = new FileInput();
        FileMessage fileMessage = new FileMessage();
        FileAddress fileAddress = new FileAddress();
        msgExchange = mock(InOnly.class);

        fileAddress.setFileDirectory("test/com/sun/jbi/filebc/input/test_recursive_root_inonly_1");

        fileAddress.setLockName(Lock.DEFAULT_INBOUND_LOCKFILE_NAME);
        fileAddress.setWorkArea(Lock.DEFAULT_INBOUND_TMPDIR_NAME);
        fileAddress.setSeqName(Lock.DEFAULT_SEQFILE_NAME);

        String archiveDir = mWorkAreaBaseDir.getAbsolutePath().concat("/" + FileMessage.FILE_ARCHIVE_DIR_DEFAULT);
        String tmpDir = mWorkAreaBaseDir.getAbsolutePath().concat("/" + Lock.DEFAULT_INBOUND_TMPDIR_NAME);
        String errDir = mWorkAreaBaseDir.getAbsolutePath() + File.separator + FileUtil.DEFAULT_ERRORS_DIR_NAME;

        cleanup(new String[]{archiveDir, tmpDir, errDir}, "Failed to clean up archive directory and filebc-in-processing directory to test execute for InOnly message exchanges: ");

        copy(new File("test/com/sun/jbi/filebc/input/recursive_root_inonly"), new File(fileAddress.getFileDirectory()));

        // Testing the following scenario
        // 1. file name is pattern and the poll is recursive
        // 2. successful "normalization"
        // 3. successful msgexchange.send
        fileAddress.setRecursive(Boolean.TRUE);
        fileAddress.setExcludeRegex(".*_skip_.*");
        fileMessage.setFileName("InputInOnly[0-9]+[a-zA-Z_]*\\.txt");
        fileMessage.setPollingInterval(new Long(5000));
        fileMessage.setFileNameIsRegex(Boolean.TRUE);
        fileInput.setFileMessage(fileMessage);

        deliveryChannel.expects(atLeastOnce()).method("createExchangeFactory").will(returnValue(msgExchangeFactory.proxy()));
        msgExchangeFactory.expects(atLeastOnce()).method("createInOnlyExchange").will(returnValue((InOnly) msgExchange.proxy()));
        componentContext.expects(atLeastOnce()).method("getEndpoint").with(eq(THE_SERVICE), eq(THE_ENDPOINT)).will(returnValue((ServiceEndpoint) serviceEndpoint.proxy()));
        msgExchange.expects(atLeastOnce()).method("getExchangeId");
        normalizer.expects(atLeastOnce()).method("normalize").withAnyArguments().will(returnValue((NormalizedMessage) normalizedMsg.proxy()));
        msgExchange.expects(atLeastOnce()).method("setEndpoint").with(isA(ServiceEndpoint.class));
        msgExchange.expects(atLeastOnce()).method("setOperation").with(eq(THE_OPERATION));
        msgExchange.expects(atLeastOnce()).method("setMessage").with(eq(normalizedMsg.proxy()), eq("in"));

        deliveryChannel.expects(atLeastOnce()).method("send").with(eq(msgExchange.proxy()));
        //endpoint.expects(atLeastOnce()).method("getServiceName").will(returnValue(THE_SERVICE));
        endpoint.expects(atLeastOnce()).method("getEndpointName").will(returnValue(THE_ENDPOINT));
        endpoint.expects(atLeastOnce()).method("getEndpointStatus").will(returnValue(endpointStatus.proxy()));
        endpoint.expects(atLeastOnce()).method("getEndpointType").will(returnValue(THE_TYPE));
        endpointStatus.expects(atLeastOnce()).method("incrementSentRequests");

        // QOS: Re-delivery
        msgExchange.expects(atLeastOnce()).method("getProperty").with(eq(FileComponentContext.CRMP_GROUP_ID)).will(returnValue(ANYTHING));
        msgExchange.expects(atLeastOnce()).method("getProperty").with(eq(FileComponentContext.CRMP_MESSAGE_ID)).will(returnValue(ANYTHING));
        deliveryChannel.expects(atLeastOnce()).method("getServiceQuality");

        // NM Properties related expectations
        normalizedMsg.expects(atLeastOnce()).method("getProperty");
        normalizedMsg.expects(atLeastOnce()).method("setProperty");

        // set no throttling
        endpoint.expects(atLeastOnce()).method("getMaxConcurrencyLimit").will(returnValue(-1));

        endpoint.expects(atLeastOnce()).method("getServiceUnitID");

        try {
            AtomicInteger maxFilesPerPoll = new AtomicInteger(0);
            InputDirFilter dirFilter = new InputDirFilter(fileAddress.getExcludeRegex());
            InputFilenameFilter fileFilter = new InputFilenameFilter(fileMessage.getFileName(),
                    fileAddress.getExcludeRegex(),
                    maxFilesPerPoll,
                    fileMessage.getFileNameIsRegex());

            instance.addWorker(worker = new IBFileWorker(instance, "inonly", fileMessage,
                    new File(fileAddress.getFileDirectory()), mWorkAreaBaseDir));
            worker.setNormalizer((FileNormalizer) normalizer.proxy());
            instance.setLock(createLock(fileAddress.getFileDirectory(), fileAddress.getLockName()));
            instance.setStopped(true); // let the worker loop once and quit
            instance.setFiles(new LinkedBlockingQueue());
            while (true) {
                boolean yield = instance.execute("inonly",
                        fileAddress.getFileDirectory(),
                        "when file filter is not null put anything here is OK - it is not used",
                        Boolean.TRUE,
                        fileInput,
                        fileAddress,
                        maxFilesPerPoll,
                        fileFilter,
                        dirFilter,maxCC);
                if (yield) {
                    Thread.sleep(50);
                } else {
                    break;
                }
            }
            Thread.sleep(30000);
            instance.setStopped(true); // signal the workers to shutdown
            // may want to check the total files staged - should equal the
            // matched input files from base dir and all sub dirs that are not excluded
            int cnt = count(tmpDir);
            assertTrue("Failed to find the expected number of input file in tmp dir [25 expected]...count=" + cnt, cnt == 25);
        } catch (Exception e) {
            e.printStackTrace();
            fail("Failed to test InOnly exchanges due to: " + e.getMessage());
        }
        instance.setWorkers(null);
        verifyContract();

        System.out.println("Successfully tested in-only message exchange case 3");
    }

    /**
     * Test of execute method, of class com.sun.jbi.filebc.InboundMessageProcessor
     * for the scenario where the message exchange type is inout.
     */
    public void testExecuteInOutExchangeCase1() {
        System.out.println("Testing in-out message exchange case 1...");

        FileInput fileInput = new FileInput();
        FileMessage fileMessage = new FileMessage();
        FileAddress fileAddress = new FileAddress();
        msgExchange = mock(InOut.class);

        fileAddress.setFileDirectory("test/com/sun/jbi/filebc/input");

        fileAddress.setLockName(Lock.DEFAULT_INBOUND_LOCKFILE_NAME);
        fileAddress.setWorkArea(Lock.DEFAULT_INBOUND_TMPDIR_NAME);
        fileAddress.setSeqName(Lock.DEFAULT_SEQFILE_NAME);

        // after cluster support changes
        // processed files are no longer marked just as <original_input_file_name>_processed and still stay at the same directory
        // instead, it is either deleted (if archive = false) or UUID tagged and _processed suffixed as <original_input_file_name><UUID>_processed and
        // moved to archive directory (if archive = true)
        String inputFile = fileAddress.getFileDirectory().concat("/InputInOut.txt");
        String testFile = fileAddress.getFileDirectory().concat("/TestInputInOut.txt");

        String archiveDir = mWorkAreaBaseDir.getAbsolutePath().concat("/" + FileMessage.FILE_ARCHIVE_DIR_DEFAULT);
        String tmpDir = mWorkAreaBaseDir.getAbsolutePath().concat("/" + Lock.DEFAULT_INBOUND_TMPDIR_NAME);
        String errDir = mWorkAreaBaseDir.getAbsolutePath() + File.separator + FileUtil.DEFAULT_ERRORS_DIR_NAME;

        cleanup(new String[]{archiveDir, tmpDir, errDir}, "Failed to clean up archive directory and filebc-in-processing directory to test execute for InOut message exchanges: ");

        validateAndPrepareTestFiles(inputFile, testFile, "Failed to create input file to test execute for InOut message exchanges: ");

        // Testing the following scenario
        // 1. no file name match pattern
        // 2. successful "normalization"
        // 3. successful msgexchange.send

        fileMessage.setFileName("TestInputInOut.txt");
        fileMessage.setPollingInterval(new Long(5000));
        fileInput.setFileMessage(fileMessage);

        deliveryChannel.expects(atLeastOnce()).method("createExchangeFactory").will(returnValue(msgExchangeFactory.proxy()));
        msgExchangeFactory.expects(atLeastOnce()).method("createInOutExchange").will(returnValue((InOut) msgExchange.proxy()));
        componentContext.expects(atLeastOnce()).method("getEndpoint").with(eq(THE_SERVICE), eq(THE_ENDPOINT)).will(returnValue((ServiceEndpoint) serviceEndpoint.proxy()));
        msgExchange.expects(atLeastOnce()).method("getExchangeId");
        normalizer.expects(atLeastOnce()).method("normalize").withAnyArguments().will(returnValue((NormalizedMessage) normalizedMsg.proxy()));
        msgExchange.expects(once()).method("setEndpoint").with(isA(ServiceEndpoint.class));
        msgExchange.expects(once()).method("setOperation").with(eq(THE_OPERATION));
        msgExchange.expects(once()).method("setMessage").with(eq(normalizedMsg.proxy()), eq("in"));

        deliveryChannel.expects(once()).method("send").with(eq(msgExchange.proxy()));
        endpoint.expects(atLeastOnce()).method("getEndpointName").will(returnValue(THE_ENDPOINT));
        endpoint.expects(once()).method("getEndpointStatus").will(returnValue(endpointStatus.proxy()));
        endpoint.expects(atLeastOnce()).method("getServiceName").will(returnValue(THE_SERVICE));
        endpoint.expects(atLeastOnce()).method("getEndpointType").will(returnValue(THE_TYPE));
        endpointStatus.expects(once()).method("incrementSentRequests");

        // QOS: Re-delivery
        msgExchange.expects(atLeastOnce()).method("getProperty").with(eq(FileComponentContext.CRMP_GROUP_ID)).will(returnValue(ANYTHING));
        msgExchange.expects(atLeastOnce()).method("getProperty").with(eq(FileComponentContext.CRMP_MESSAGE_ID)).will(returnValue(ANYTHING));
        deliveryChannel.expects(once()).method("getServiceQuality");

        // NM Properties related expectations
        normalizedMsg.expects(atLeastOnce()).method("getProperty");
        normalizedMsg.expects(atLeastOnce()).method("setProperty");

        // set no throttling
        endpoint.expects(atLeastOnce()).method("getMaxConcurrencyLimit").will(returnValue(-1));

        endpoint.expects(atLeastOnce()).method("getServiceUnitID");

        try {
            AtomicInteger maxFilesPerPoll = new AtomicInteger(20);
            InputDirFilter dirFilter = /*new InputDirFilter("")*/ null;
            InputFilenameFilter fileFilter = /*new InputFilenameFilter(fileInput.getFileMessage().getFileName(), "", maxFilesPerPoll, false)*/ null;

            instance.addWorker(worker = new IBFileWorker(instance, "inout", fileMessage,
                    new File(fileAddress.getFileDirectory()), mWorkAreaBaseDir));
            worker.setNormalizer((FileNormalizer) normalizer.proxy());
            instance.setLock(createLock(fileAddress.getFileDirectory(), fileAddress.getLockName()));
            instance.setStopped(true); // let the worker loop once and quit
            instance.setFiles(new LinkedBlockingQueue());
            instance.execute("inout",
                    fileAddress.getFileDirectory(),
                    "TestInputInOut.txt",
                    Boolean.FALSE,
                    fileInput,
                    fileAddress,
                    maxFilesPerPoll,
                    fileFilter,
                    dirFilter,maxCC);
            Thread.sleep(5000);
            assertTrue("Failed to find the input file in tmp dir...", hasFilesWithSuffix(tmpDir, ""));
        } catch (Exception e) {
            fail("Failed to test InOnly exchanges due to: " + e.getMessage());
        }
        verifyContract();

        // Testing the following scenario
        // 1. no file name match pattern
        // 2. Unsuccessful "normalization"
        cleanup(new String[]{archiveDir, tmpDir, errDir}, "Failed to clean up archive directory and filebc-in-processing directory to test execute for InOut message exchanges: ");
        validateAndPrepareTestFiles(inputFile, testFile, "Failed to create input file to test execute for InOut message exchanges: ");

        normalizer.expects(atLeastOnce()).method("normalize").withAnyArguments().will(throwException(new Exception("Failed to normalize")));

        try {
            AtomicInteger maxFilesPerPoll = new AtomicInteger(20);
            InputDirFilter dirFilter = /*new InputDirFilter("")*/ null;
            InputFilenameFilter fileFilter = /*new InputFilenameFilter(fileInput.getFileMessage().getFileName(), "", maxFilesPerPoll, false)*/ null;

            instance.setWorkers(null);
            instance.addWorker(worker = new IBFileWorker(instance, "inout", fileMessage,
                    new File(fileAddress.getFileDirectory()), mWorkAreaBaseDir));
            worker.setNormalizer((FileNormalizer) normalizer.proxy());
            instance.setLock(createLock(fileAddress.getFileDirectory(), fileAddress.getLockName()));
            instance.setStopped(true); // let the worker loop once and quit
            instance.execute("inout",
                    fileAddress.getFileDirectory(),
                    "TestInputInOut.txt",
                    Boolean.FALSE,
                    fileInput,
                    fileAddress,
                    maxFilesPerPoll,
                    fileFilter,
                    dirFilter,maxCC);

            Thread.sleep(5000);
            // now there are worker threads spin off by execute(), the exception "Failed to normalize" is thrown
            // in the run method of IBFileWorker and is captured and logged and
            // the offending message is marked as <original_input_file_name><UUID>_error and will stay in the
            // work area - tmp dir
            // the next best assertion for this scenario is to assert that the _error file is there
            assertTrue("Failed to find the _error suffixed input file in error dir...", hasFilesWithSuffix(errDir, FileUtil.getErrorFileSuffix()));
            //fail("Failed to test in-out message exchange when an expected exception should be caught");
        } catch (Exception e) {
            fail("Failed to test execute for In-Out message exchanges with Exception 'Failed to normalize': " + e.getMessage());
            //System.out.println("Caught the expected exception!!");
        }
        verifyContract();
        System.out.println("Successfully tested in-out message exchange case 1");
    }

    /**
     * Test of execute method, of class com.sun.jbi.filebc.InboundMessageProcessor
     * for the scenario where the message exchange type is inout.
     */
    public void testExecuteInOutExchangeCase3() {
        System.out.println("Testing in-out message exchange case 3...");

        FileInput fileInput = new FileInput();
        FileMessage fileMessage = new FileMessage();
        FileAddress fileAddress = new FileAddress();
        msgExchange = mock(InOut.class);

        fileAddress.setFileDirectory("test/com/sun/jbi/filebc/input/test_recursive_root_inout_1");

        fileAddress.setLockName(Lock.DEFAULT_INBOUND_LOCKFILE_NAME);
        fileAddress.setWorkArea(Lock.DEFAULT_INBOUND_TMPDIR_NAME);
        fileAddress.setSeqName(Lock.DEFAULT_SEQFILE_NAME);

        String archiveDir = mWorkAreaBaseDir.getAbsolutePath().concat("/" + FileMessage.FILE_ARCHIVE_DIR_DEFAULT);
        String tmpDir = mWorkAreaBaseDir.getAbsolutePath().concat("/" + Lock.DEFAULT_INBOUND_TMPDIR_NAME);
        String errDir = mWorkAreaBaseDir.getAbsolutePath() + File.separator + FileUtil.DEFAULT_ERRORS_DIR_NAME;

        cleanup(new String[]{archiveDir, tmpDir, errDir}, "Failed to clean up archive directory and filebc-in-processing directory to test execute for InOut message exchanges: ");

        copy(new File("test/com/sun/jbi/filebc/input/recursive_root_inout"), new File(fileAddress.getFileDirectory()));

        // Testing the following scenario
        // 1. file name is regular expression and the poll is recursive with exclude regex
        // 2. successful "normalization"
        // 3. successful msgexchange.send

        fileAddress.setRecursive(Boolean.TRUE);
        fileAddress.setExcludeRegex(".*_skip_.*");
        fileMessage.setFileName("InputInOut[0-9]+[a-zA-Z_]*\\.dat");
        fileMessage.setFileNameIsRegex(Boolean.TRUE);
        fileMessage.setPollingInterval(new Long(5000));
        fileInput.setFileMessage(fileMessage);

        deliveryChannel.expects(atLeastOnce()).method("createExchangeFactory").will(returnValue(msgExchangeFactory.proxy()));
        msgExchangeFactory.expects(atLeastOnce()).method("createInOutExchange").will(returnValue((InOut) msgExchange.proxy()));
        componentContext.expects(atLeastOnce()).method("getEndpoint").with(eq(THE_SERVICE), eq(THE_ENDPOINT)).will(returnValue((ServiceEndpoint) serviceEndpoint.proxy()));
        msgExchange.expects(atLeastOnce()).method("getExchangeId");
        normalizer.expects(atLeastOnce()).method("normalize").withAnyArguments().will(returnValue((NormalizedMessage) normalizedMsg.proxy()));
        msgExchange.expects(atLeastOnce()).method("setEndpoint").with(isA(ServiceEndpoint.class));
        msgExchange.expects(atLeastOnce()).method("setOperation").with(eq(THE_OPERATION));
        msgExchange.expects(atLeastOnce()).method("setMessage").with(eq(normalizedMsg.proxy()), eq("in"));

        deliveryChannel.expects(atLeastOnce()).method("send").with(eq(msgExchange.proxy()));
        endpoint.expects(atLeastOnce()).method("getEndpointName").will(returnValue(THE_ENDPOINT));
        endpoint.expects(atLeastOnce()).method("getEndpointStatus").will(returnValue(endpointStatus.proxy()));
        endpoint.expects(atLeastOnce()).method("getServiceName").will(returnValue(THE_SERVICE));
        endpoint.expects(atLeastOnce()).method("getEndpointType").will(returnValue(THE_TYPE));
        endpointStatus.expects(atLeastOnce()).method("incrementSentRequests");

        // QOS: Re-delivery
        msgExchange.expects(atLeastOnce()).method("getProperty").with(eq(FileComponentContext.CRMP_GROUP_ID)).will(returnValue(ANYTHING));
        msgExchange.expects(atLeastOnce()).method("getProperty").with(eq(FileComponentContext.CRMP_MESSAGE_ID)).will(returnValue(ANYTHING));
        deliveryChannel.expects(atLeastOnce()).method("getServiceQuality");

        // NM Properties related expectations
        normalizedMsg.expects(atLeastOnce()).method("getProperty");
        normalizedMsg.expects(atLeastOnce()).method("setProperty");

        // set no throttling
        endpoint.expects(atLeastOnce()).method("getMaxConcurrencyLimit").will(returnValue(-1));

        endpoint.expects(atLeastOnce()).method("getServiceUnitID");

        try {
            AtomicInteger maxFilesPerPoll = new AtomicInteger(0);
            InputDirFilter dirFilter = new InputDirFilter(fileAddress.getExcludeRegex());
            InputFilenameFilter fileFilter = new InputFilenameFilter(fileMessage.getFileName(),
                    fileAddress.getExcludeRegex(),
                    maxFilesPerPoll,
                    fileMessage.getFileNameIsRegex());
            instance.addWorker(worker = new IBFileWorker(instance, "inout", fileMessage,
                    new File(fileAddress.getFileDirectory()), mWorkAreaBaseDir));
            worker.setNormalizer((FileNormalizer) normalizer.proxy());
            instance.setLock(createLock(fileAddress.getFileDirectory(), fileAddress.getLockName()));
            instance.setFiles(new LinkedBlockingQueue());
            while (true) {
                boolean yield = instance.execute("inout",
                        fileAddress.getFileDirectory(), // the base dir to be polled (recursively)
                        "when file filter is not null put anything here is OK - it is not used",
                        Boolean.FALSE,
                        fileInput,
                        fileAddress,
                        maxFilesPerPoll,
                        fileFilter,
                        dirFilter,maxCC);
                if (yield) {
                    Thread.sleep(50);
                } else {
                    break;
                }
            }
            Thread.sleep(30000);
            instance.setStopped(true); // signal the workers to shutdown
            // may want to check the total files staged - should equal the
            // matched input files from base dir and all sub dirs that are not excluded
            int cnt = count(tmpDir);
            assertTrue("Failed to find the expected number of input file in tmp dir [25 expected]...count=" + cnt, cnt == 25);
        } catch (Exception e) {
            fail("Failed to test InOnly exchanges due to: " + e.getMessage());
        }

        verifyContract();

        // Testing the following scenario
        // 1. file name is pattern, poll is recursive
        // 2. successful "normalization"
        // 3. successful msgexchange.send
        cleanup(new String[]{archiveDir, tmpDir, errDir}, "Failed to clean up archive directory and filebc-in-processing directory to test execute for InOut message exchanges: ");
        instance.setWorkers(null);

        fileAddress.setFileDirectory("test/com/sun/jbi/filebc/input/test_recursive_root_inout_2");

        fileMessage.setFileName("InputInOut%d.dat");
        fileMessage.setFileNameIsRegex(Boolean.FALSE);
        fileMessage.setFileNameIsPattern(Boolean.TRUE);

        copy(new File("test/com/sun/jbi/filebc/input/recursive_root_inout"), new File(fileAddress.getFileDirectory()));

        try {
            AtomicInteger maxFilesPerPoll = new AtomicInteger(0);
            InputDirFilter dirFilter = new InputDirFilter(fileAddress.getExcludeRegex());
            InputFilenameFilter fileFilter = new InputFilenameFilter(fileMessage.getFileName(),
                    fileAddress.getExcludeRegex(),
                    maxFilesPerPoll,
                    fileMessage.getFileNameIsRegex().booleanValue());
            instance.addWorker(worker = new IBFileWorker(instance, "inout", fileMessage,
                    new File(fileAddress.getFileDirectory()), mWorkAreaBaseDir));
            worker.setNormalizer((FileNormalizer) normalizer.proxy());
            instance.setLock(createLock(fileAddress.getFileDirectory(), fileAddress.getLockName()));
            instance.setFiles(new LinkedBlockingQueue());
            while (true) {
                boolean yield = instance.execute("inout",
                        fileAddress.getFileDirectory(), // the base dir to be polled (recursively)
                        "when file filter is not null put anything here is OK - it is not used",
                        Boolean.FALSE,
                        fileInput,
                        fileAddress,
                        maxFilesPerPoll,
                        fileFilter,
                        dirFilter,maxCC);
                if (yield) {
                    Thread.sleep(50);
                } else {
                    break;
                }
            }
            Thread.sleep(30000);
            instance.setStopped(true); // signal the workers to shutdown
            int cnt = count(tmpDir);
            assertTrue("Failed to find the expected number of input file in tmp dir [25 expected]...count=" + cnt, cnt == 25);
        } catch (Exception e) {
            e.printStackTrace();
            fail("Failed to test InOnly exchanges due to: " + e.getMessage());
        }

        verifyContract();
        System.out.println("Successfully tested in-out message exchange case 3");
    }

    /**
     * Test of execute method, of class com.sun.jbi.filebc.InboundMessageProcessor
     * for the scenario where the message exchange type is inout.
     */
    public void testExecuteInOutExchangeCase2() {
        System.out.println("Testing in-out message exchange case 2...");

        FileInput fileInput = new FileInput();
        FileMessage fileMessage = new FileMessage();
        FileAddress fileAddress = new FileAddress();
        msgExchange = mock(InOut.class);

        fileAddress.setFileDirectory("test/com/sun/jbi/filebc/input");

        fileAddress.setLockName(Lock.DEFAULT_INBOUND_LOCKFILE_NAME);
        fileAddress.setWorkArea(Lock.DEFAULT_INBOUND_TMPDIR_NAME);
        fileAddress.setSeqName(Lock.DEFAULT_SEQFILE_NAME);

        // after cluster support changes
        // processed files are no longer marked just as <original_input_file_name>_processed and still stay at the same directory
        // instead, it is either deleted (if archive = false) or UUID tagged and _processed suffixed as <original_input_file_name><UUID>_processed and
        // moved to archive directory (if archive = true)
        String inputFile = fileAddress.getFileDirectory().concat("/InputInOut.txt");
        String testFile = fileAddress.getFileDirectory().concat("/I8n.dat");

        String archiveDir = mWorkAreaBaseDir.getAbsolutePath().concat("/" + FileMessage.FILE_ARCHIVE_DIR_DEFAULT);
        String tmpDir = mWorkAreaBaseDir.getAbsolutePath().concat("/" + Lock.DEFAULT_INBOUND_TMPDIR_NAME);
        String errDir = mWorkAreaBaseDir.getAbsolutePath() + File.separator + FileUtil.DEFAULT_ERRORS_DIR_NAME;

        cleanup(new String[]{archiveDir, tmpDir, errDir}, "Failed to clean up archive directory and filebc-in-processing directory to test execute for InOut message exchanges: ");
        validateAndPrepareTestFiles(inputFile, testFile, "Failed to create input file to test execute for InOut message exchanges: ");

        // Testing the following scenario
        // 1. no file name match pattern
        // 2. successful "normalization"
        // 3. successful msgexchange.send

        fileMessage.setFileName("[A-Z][0-9]n.[a-e]a[t-z]");
        fileMessage.setPollingInterval(new Long(5000));
        fileInput.setFileMessage(fileMessage);

        deliveryChannel.expects(atLeastOnce()).method("createExchangeFactory").will(returnValue(msgExchangeFactory.proxy()));
        msgExchangeFactory.expects(atLeastOnce()).method("createInOutExchange").will(returnValue((InOut) msgExchange.proxy()));
        componentContext.expects(atLeastOnce()).method("getEndpoint").with(eq(THE_SERVICE), eq(THE_ENDPOINT)).will(returnValue((ServiceEndpoint) serviceEndpoint.proxy()));
        msgExchange.expects(atLeastOnce()).method("getExchangeId");
        normalizer.expects(atLeastOnce()).method("normalize").withAnyArguments().will(returnValue((NormalizedMessage) normalizedMsg.proxy()));
        msgExchange.expects(once()).method("setEndpoint").with(isA(ServiceEndpoint.class));
        msgExchange.expects(once()).method("setOperation").with(eq(THE_OPERATION));
        msgExchange.expects(once()).method("setMessage").with(eq(normalizedMsg.proxy()), eq("in"));

        deliveryChannel.expects(once()).method("send").with(eq(msgExchange.proxy()));
        //endpoint.expects(atLeastOnce()).method("getServiceName").will(returnValue(THE_SERVICE));
        endpoint.expects(atLeastOnce()).method("getEndpointName").will(returnValue(THE_ENDPOINT));
        endpoint.expects(once()).method("getEndpointStatus").will(returnValue(endpointStatus.proxy()));
        endpoint.expects(atLeastOnce()).method("getEndpointType").will(returnValue(THE_TYPE));
        endpointStatus.expects(once()).method("incrementSentRequests");

        // QOS: Re-delivery
        msgExchange.expects(atLeastOnce()).method("getProperty").with(eq(FileComponentContext.CRMP_GROUP_ID)).will(returnValue(ANYTHING));
        msgExchange.expects(atLeastOnce()).method("getProperty").with(eq(FileComponentContext.CRMP_MESSAGE_ID)).will(returnValue(ANYTHING));
        deliveryChannel.expects(once()).method("getServiceQuality");

        // NM Properties related expectations
        normalizedMsg.expects(atLeastOnce()).method("getProperty");
        normalizedMsg.expects(atLeastOnce()).method("setProperty");

        // set no throttling
        endpoint.expects(atLeastOnce()).method("getMaxConcurrencyLimit").will(returnValue(-1));

        endpoint.expects(atLeastOnce()).method("getServiceUnitID");

        try {
            AtomicInteger maxFilesPerPoll = new AtomicInteger(20);
            InputDirFilter dirFilter = /*new InputDirFilter("")*/ null;
            InputFilenameFilter fileFilter = new InputFilenameFilter(fileInput.getFileMessage().getFileName(), "", maxFilesPerPoll, true);

            instance.addWorker(worker = new IBFileWorker(instance, "inout", fileMessage,
                    new File(fileAddress.getFileDirectory()), mWorkAreaBaseDir));
            worker.setNormalizer((FileNormalizer) normalizer.proxy());
            instance.setLock(createLock(fileAddress.getFileDirectory(), fileAddress.getLockName()));
            instance.setStopped(true); // let the worker loop once and quit
            instance.setFiles(new LinkedBlockingQueue());
            fileFilter.setFilterExpression("I%dn.dat");
            instance.execute("inout",
                    fileAddress.getFileDirectory(),
                    "I%dn.dat",
                    Boolean.TRUE,
                    fileInput,
                    fileAddress,
                    maxFilesPerPoll,
                    fileFilter,
                    dirFilter,maxCC);

            Thread.sleep(5000);
            assertTrue("Failed to find the input file in tmp dir...", hasFilesWithSuffix(tmpDir, ""));
        } catch (Exception e) {
            fail("Failed to test InOnly exchanges due to: " + e.getMessage());
        }
        verifyContract();

        // Testing the following scenario
        // 1. no file name match pattern
        // 2. Unsuccessful "normalization"
        cleanup(new String[]{archiveDir, tmpDir, errDir}, "Failed to clean up archive directory and filebc-in-processing directory to test execute for InOut message exchanges: ");
        validateAndPrepareTestFiles(inputFile, testFile, "Failed to create input file to test execute for InOut message exchanges: ");

        normalizer.expects(atLeastOnce()).method("normalize").withAnyArguments().will(throwException(new Exception("Failed to normalize")));

        try {
            AtomicInteger maxFilesPerPoll = new AtomicInteger(20);
            InputDirFilter dirFilter = /*new InputDirFilter("")*/ null;
            InputFilenameFilter fileFilter = new InputFilenameFilter(fileInput.getFileMessage().getFileName(), "", maxFilesPerPoll, true);

            instance.setWorkers(null);
            instance.addWorker(worker = new IBFileWorker(instance, "inout", fileMessage,
                    new File(fileAddress.getFileDirectory()), mWorkAreaBaseDir));
            worker.setNormalizer((FileNormalizer) normalizer.proxy());
            instance.setLock(createLock(fileAddress.getFileDirectory(), fileAddress.getLockName()));
            instance.setStopped(true); // let the worker loop once and quit
            fileFilter.setFilterExpression("I%dn.dat");
            instance.execute("inout",
                    fileAddress.getFileDirectory(),
                    "I%dn.dat",
                    Boolean.TRUE,
                    fileInput,
                    fileAddress,
                    maxFilesPerPoll,
                    fileFilter,
                    dirFilter,maxCC);
            Thread.sleep(5000);
            // now there are worker threads spin off by execute(), the exception "Failed to normalize" is thrown
            // in the run method of IBFileWorker and is captured and logged and
            // the offending message is marked as <original_input_file_name><UUID>_error and will stay in the
            // work area - tmp dir
            // the next best assertion for this scenario is to assert that the _error file is there
            assertTrue("Failed to find the _error suffixed input file in error dir...", hasFilesWithSuffix(errDir, FileUtil.getErrorFileSuffix()));
            //fail("Failed to test in-out message exchange when an expected exception should be caught");
        } catch (Exception e) {
            fail("Failed to test execute for In-Out message exchanges with Exception 'Failed to normalize': " + e.getMessage());
            //System.out.println("Caught the expected exception!!");
        }
        verifyContract();

        System.out.println("Successfully tested in-out message exchange case 2");
    }

    /**
     * Test of processReplyMessage method, of class com.sun.jbi.filebc.InboundMessageProcessor.
     */
    public void testProcessReplyMessage() {
        System.out.println("Testing processReplyMessage");

        FileInput fileInput = new FileInput();
        FileMessage fileMessage = new FileMessage();

        fileMessage.setFileName("TestInputInOut.txt");
        fileMessage.setPollingInterval(new Long(5000));
        fileInput.setFileMessage(fileMessage);

        FileAddress fileAddress = new FileAddress();
        fileAddress.setFileDirectory("test/com/sun/jbi/filebc/input");
        fileAddress.setLockName(Lock.DEFAULT_INBOUND_LOCKFILE_NAME);
        fileAddress.setWorkArea(Lock.DEFAULT_INBOUND_TMPDIR_NAME);
        fileAddress.setSeqName(Lock.DEFAULT_SEQFILE_NAME);

        Map inboundReplys = new HashMap();
        Map Ids = new HashMap();

        msgExchange = mock(MessageExchange.class);
        msgExchange.stubs().method("getPattern");

        // testing invalid message exchange
        try {
            instance.processReplyMessage((MessageExchange) msgExchange.proxy());
            fail("Failed to test processReplyMessage when an validation exception should be caught.");
        } catch (Exception e) {
            System.out.println("Successfully tested processReply when the MessageExchange is neither in-out nor in-only");
        }

        // testing no exchangeId found
        msgExchange = mock(InOut.class);
        msgExchange.expects(once()).method("getExchangeId").will(returnValue("789"));
        Ids.put("123", "");
        instance.setInboundExchangeIds(Ids);

        try {
            instance.processReplyMessage((InOut) msgExchange.proxy());
            System.out.println("Successfully tested processReply when message exchange ID is not found");
        } catch (Exception e) {
            fail("Failed to test processReplyMessage when message exchange ID is not found.");
        }

        // testing exchang Id is found but status is not DONE
        String origFile = fileAddress.getFileDirectory().concat("/InputInOut.txt");
        String inputFile = fileAddress.getFileDirectory().concat("/TestInputInOut.txt");

        String archiveDir = mWorkAreaBaseDir.getAbsolutePath().concat("/" + FileMessage.FILE_ARCHIVE_DIR_DEFAULT);
        String tmpDir = mWorkAreaBaseDir.getAbsolutePath().concat("/" + Lock.DEFAULT_INBOUND_TMPDIR_NAME);
        String errDir = mWorkAreaBaseDir.getAbsolutePath() + File.separator + FileUtil.DEFAULT_ERRORS_DIR_NAME;

        // after cluster support changes
        // processed files are no longer marked just as <original_input_file_name>_processed and still stay at the same directory
        // instead, it is either deleted (if archive = false) or UUID tagged and _processed suffixed as <original_input_file_name><UUID>_processed and
        // moved to archive directory (if archive = true)
        // now the error marked file stays in tmp directory

        cleanup(new String[]{archiveDir, tmpDir, errDir}, "Failed to clean up archive directory and filebc-in-processing directory to test processReplyMessage: ");
        validateAndPrepareTestFiles(origFile, inputFile, "Failed to create a input file to properly test processReplyMessage method - ID found and status = DONE: ");

        deliveryChannel.expects(atLeastOnce()).method("createExchangeFactory").will(returnValue(msgExchangeFactory.proxy()));
        msgExchangeFactory.expects(atLeastOnce()).method("createInOutExchange").will(returnValue((InOut) msgExchange.proxy()));
        componentContext.expects(atLeastOnce()).method("getEndpoint").with(eq(THE_SERVICE), eq(THE_ENDPOINT)).will(returnValue((ServiceEndpoint) serviceEndpoint.proxy()));
        msgExchange.expects(atLeastOnce()).method("getExchangeId");
        normalizer.expects(atLeastOnce()).method("normalize").withAnyArguments().will(returnValue((NormalizedMessage) normalizedMsg.proxy()));
        msgExchange.expects(once()).method("setEndpoint").with(isA(ServiceEndpoint.class));
        msgExchange.expects(once()).method("setOperation").with(eq(THE_OPERATION));
        msgExchange.expects(once()).method("setMessage").with(eq(normalizedMsg.proxy()), eq("in"));

        deliveryChannel.expects(once()).method("send").with(eq(msgExchange.proxy()));
        //endpoint.expects(atLeastOnce()).method("getServiceName").will(returnValue(THE_SERVICE));
        endpoint.expects(atLeastOnce()).method("getEndpointName").will(returnValue(THE_ENDPOINT));
        endpoint.expects(atLeastOnce()).method("getEndpointType").will(returnValue(THE_TYPE));
        endpoint.expects(once()).method("getEndpointStatus").will(returnValue(endpointStatus.proxy()));
        endpointStatus.expects(once()).method("incrementSentRequests");

        // QOS: Re-delivery
        msgExchange.expects(atLeastOnce()).method("getProperty").with(eq(FileComponentContext.CRMP_GROUP_ID)).will(returnValue(ANYTHING));
        msgExchange.expects(atLeastOnce()).method("getProperty").with(eq(FileComponentContext.CRMP_MESSAGE_ID)).will(returnValue(ANYTHING));
        deliveryChannel.expects(atLeastOnce()).method("getServiceQuality");

        normalizedMsg.expects(atLeastOnce()).method("getProperty");
        normalizedMsg.expects(atLeastOnce()).method("setProperty");

        // set no throttling
        endpoint.expects(atLeastOnce()).method("getMaxConcurrencyLimit").will(returnValue(-1));

        endpoint.expects(atLeastOnce()).method("getServiceUnitID");

        try {
            AtomicInteger maxFilesPerPoll = new AtomicInteger(20);
            InputDirFilter dirFilter = /*new InputDirFilter("")*/ null;
            InputFilenameFilter fileFilter = /*new InputFilenameFilter(fileInput.getFileMessage().getFileName(), "", maxFilesPerPoll, false)*/ null;

            instance.setWorkers(null);
            instance.addWorker(worker = new IBFileWorker(instance, "inout", fileMessage,
                    new File(fileAddress.getFileDirectory()), mWorkAreaBaseDir));
            worker.setNormalizer((FileNormalizer) normalizer.proxy());
            instance.setFileMessage(fileMessage);
            instance.setTargetDirectory(fileAddress.getFileDirectory());
            instance.setLock(createLock(fileAddress.getFileDirectory(), fileAddress.getLockName()));
            instance.setStopped(true); // let the worker loop once and quit
            instance.setFiles(new LinkedBlockingQueue());
            instance.execute("inout",
                    fileAddress.getFileDirectory(),
                    "TestInputInOut.txt",
                    Boolean.FALSE,
                    fileInput,
                    fileAddress,
                    maxFilesPerPoll,
                    fileFilter,
                    dirFilter,maxCC);
            Thread.sleep(5000);
            assertTrue("Failed to detect a UUID tagged input file in work area - tmp dir when preparing for replyMessage() test [ID found status = DONE]", hasFilesWithSuffix(tmpDir, ""));
        } catch (Exception e) {
            fail("Failed to test replyMessage when call InOnly exchanges to prepare some expected files due to: " + e.getMessage());
        }

        File tmpDirObj = new File(tmpDir);
        MyFilter myFilter = new MyFilter(fileMessage.getFileName());
        File[] tmpFiles = tmpDirObj.listFiles(myFilter);

        if (tmpFiles == null || tmpFiles.length == 0) {
            fail("Failed to test replyMessage due to the UUID tagged input file does not shown up in work area - tmp dir - for [ID found status = DONE]");
        }

        // testing exchange Id is found and status is DONE
        FileMeta fileMeta = new FileMeta(tmpFiles[0]);
        fileMeta.setNMProperty(FileMeta.NMPROP_INBOUND_FILEDIR, tmpDirObj.getAbsolutePath());
        fileMeta.setNMProperty(FileMeta.NMPROP_INBOUND_FILENAME, tmpFiles[0].getName());
        inboundReplys.put("123", fileMeta);
        instance.setInboundReplyIds(inboundReplys);
        Ids.put("123", "");
        instance.setInboundExchangeIds(Ids);
        msgExchange.expects(once()).method("getExchangeId").will(returnValue("123"));
        msgExchange.expects(once()).method("getStatus").will(returnValue(ExchangeStatus.DONE));
        try {
            instance.processReplyMessage((InOut) msgExchange.proxy());
            Map inboundIds = instance.getInboundExchanges();
            // the id will be removed after the reply is processed
            assertTrue("'123' should be removed by now - but it is not", !inboundIds.containsKey("123"));
            // the processed input file should be moved to archive area with suffix _processed
            assertTrue("Failed to assert that there is an _processed suffixed file in working area - tmp dir", hasFilesWithSuffix(archiveDir, "_processed"));
        } catch (Exception e) {
            e.printStackTrace();
            fail("Failed to test processReplyMessage when message exchange status is DONE.");
        }

        cleanup(new String[]{archiveDir, tmpDir, errDir}, "Failed to clean up archive directory and filebc-in-processing directory to test processReplyMessage: ");
        validateAndPrepareTestFiles(origFile, inputFile, "Failed to create a input file to properly test processReplyMessage method: ");

        // provide the UUID tagged file for replyMessage() to do post processing - archive or delete or mark as error
        //deliveryChannel.expects(atLeastOnce()).method("createExchangeFactory").will(returnValue(msgExchangeFactory.proxy()));
        msgExchangeFactory.expects(atLeastOnce()).method("createInOutExchange").will(returnValue((InOut) msgExchange.proxy()));
        //componentContext.expects(atLeastOnce()).method("getEndpoint").with(eq(THE_SERVICE), eq(THE_ENDPOINT)).will(returnValue((ServiceEndpoint)serviceEndpoint.proxy()));
        msgExchange.expects(atLeastOnce()).method("getExchangeId");
        normalizer.expects(atLeastOnce()).method("normalize").withAnyArguments().will(returnValue((NormalizedMessage) normalizedMsg.proxy()));
        msgExchange.expects(once()).method("setEndpoint").with(isA(ServiceEndpoint.class));
        msgExchange.expects(once()).method("setOperation").with(eq(THE_OPERATION));
        msgExchange.expects(once()).method("setMessage").with(eq(normalizedMsg.proxy()), eq("in"));

        deliveryChannel.expects(once()).method("send").with(eq(msgExchange.proxy()));
        //endpoint.expects(atLeastOnce()).method("getServiceName").will(returnValue(THE_SERVICE));
        //endpoint.expects(atLeastOnce()).method("getEndpointName").will(returnValue(THE_ENDPOINT));
        endpoint.expects(once()).method("getEndpointStatus").will(returnValue(endpointStatus.proxy()));
        endpointStatus.expects(once()).method("incrementSentRequests");

        try {
            AtomicInteger maxFilesPerPoll = new AtomicInteger(20);
            InputDirFilter dirFilter = /*new InputDirFilter("")*/ null;
            InputFilenameFilter fileFilter = /*new InputFilenameFilter(fileInput.getFileMessage().getFileName(), "", maxFilesPerPoll, false)*/ null;

            instance.setWorkers(null);
            instance.addWorker(worker = new IBFileWorker(instance, "inout", fileMessage,
                    new File(fileAddress.getFileDirectory()), mWorkAreaBaseDir));
            worker.setNormalizer((FileNormalizer) normalizer.proxy());
            instance.setFileMessage(fileMessage);
            instance.setTargetDirectory(fileAddress.getFileDirectory());
            instance.setLock(createLock(fileAddress.getFileDirectory(), fileAddress.getLockName()));
            instance.setStopped(true); // let the worker loop once and quit
            instance.execute("inout",
                    fileAddress.getFileDirectory(),
                    "TestInputInOut.txt",
                    Boolean.FALSE,
                    fileInput,
                    fileAddress,
                    maxFilesPerPoll,
                    fileFilter,
                    dirFilter,maxCC);
            Thread.sleep(5000);
            assertTrue("Failed to detect a UUID tagged input file in work area - tmp dir when preparing for replyMessage() test [ID found status != DONE]", hasFilesWithSuffix(tmpDir, ""));
        } catch (Exception e) {
            fail("Failed to test replyMessage when call InOnly exchanges to prepare some expected files due to: " + e.getMessage());
        }

        tmpFiles = tmpDirObj.listFiles(myFilter);
        if (tmpFiles == null || tmpFiles.length == 0) {
            fail("Failed to test replyMessage due to the UUID tagged input file does not shown up in work area - tmp dir - for [ID found status != DONE]");
        }

        fileMeta = new FileMeta(tmpFiles[0]);
        fileMeta.setNMProperty(FileMeta.NMPROP_INBOUND_FILEDIR, tmpDirObj.getAbsolutePath());
        fileMeta.setNMProperty(FileMeta.NMPROP_INBOUND_FILENAME, tmpFiles[0].getName());
        inboundReplys.put("123", fileMeta);
        instance.setInboundReplyIds(inboundReplys);
        Ids.put("123", "");
        instance.setInboundExchangeIds(Ids);
        msgExchange.expects(once()).method("getExchangeId").will(returnValue("123"));
        msgExchange.expects(once()).method("getStatus").will(returnValue(ExchangeStatus.ERROR));
        Mock serviceEndpoint = mock(ServiceEndpoint.class);
        msgExchange.expects(atLeastOnce()).method("getEndpoint").will(returnValue(serviceEndpoint.proxy()));
        serviceEndpoint.expects(atLeastOnce()).method("getServiceName");
        serviceEndpoint.expects(atLeastOnce()).method("getEndpointName");
        msgExchange.expects(once()).method("getOperation");
        msgExchange.expects(atLeastOnce()).method("getProperty");
        msgExchange.expects(once()).method("getError");

        try {
            instance.processReplyMessage((InOut) msgExchange.proxy());
            Map inboundIds = instance.getInboundExchanges();
            assertTrue("Failed to assert that Inbound Exchange ID 123 are removed", !inboundIds.containsKey("123"));
            // in the tmp directory, there should be the UUID tagged input file with _error suffix
            assertTrue("Failed to assert that there is an _error suffixed file in error dir", hasFilesWithSuffix(errDir, FileUtil.getErrorFileSuffix()));
            // processReplyMessage(...) encountered an error and marked input file with _error
            // accordingly
            System.out.println("Successfully tested processReply when message exchange status is not DONE.");
        } catch (Exception e) {
            e.printStackTrace();
            fail("Failed to test processReplyMessage when message exchange status is not DONE.");
        }

        System.out.println("Successfully tested processReplyMessage");
    }

    private void copy(File srcRoot, File destRoot) {
        if (!srcRoot.exists()) {
            fail("Original directory does not exist when preparing test data..., src=" + srcRoot + " dest=" + destRoot);
        }
        destRoot.mkdirs();
        File[] entries = srcRoot.listFiles();
        File entry = null;
        for (int i = 0; i < entries.length; i++) {
            entry = entries[i];
            if (entry.isDirectory()) {
                copy(entry, new File(destRoot, entry.getName()));
            } else {
                copyFile(entry, new File(destRoot, entry.getName()));
            }
        }
    }

    private int count(String dir) {
        int result = 0;
        File dirObj = new File(dir);
        File[] entries = dirObj.listFiles();
        if (entries != null) {
            for (int i = 0; i < entries.length; i++) {
                if (entries[i].isFile()) {
                    result++;
                }
            }
        }
        return result;
    }

    private void copyFile(String fromFile, String toFile) {
        copyFile(new File(fromFile), new File(toFile));
    }

    private void copyFile(File fromFile, File toFile) {
        StringBuffer output = new StringBuffer();
        try {
            java.io.FileReader reader = new java.io.FileReader(fromFile);
            char[] buff = new char[512];
            int len = reader.read(buff);
            int length = len;
            while (len > 0) {
                output.append(buff, 0, len);
                len = reader.read(buff);
            }
            java.io.FileWriter writer = new java.io.FileWriter(toFile);
            writer.write(buff, 0, length);
            writer.flush();
            writer.close();
        } catch (Exception e) {
            System.err.println("Failed to copy " + fromFile + " to " + toFile);
        }
    }

    private Lock createLock(String dir, String lck) throws Exception {
        File lf = new File(dir, lck);
        if (!lf.exists()) {
            lf.createNewFile();
        }
        FileOutputStream fos = new FileOutputStream(lf);

        return new Lock(fos.getChannel(), new ReentrantLock(), lf.getCanonicalPath());
    }

    private void cleanUpDir(String dir) {
        // remove all the files under the given directory
        File d = new File(dir);
        if (d.exists()) {
            File[] files = d.listFiles();
            for (int i = 0; i < files.length; i++) {
                files[i].delete();
            }
        }
    }

    private boolean hasFilesWithSuffix(String dir, String suffix) {
        File d = new File(dir);
        boolean result = false;
        if (d.exists()) {
            File[] files = d.listFiles();
            for (int i = 0; i < files.length; i++) {
                if (suffix == null && suffix.trim().length() == 0) {
                    result = true;
                    break;
                }
                if (files[i].getName().endsWith(suffix)) {
                    result = true;
                    break;
                }
            }
        }
        return result;
    }

    private void verifyContract() {
        msgExchange.verify();
        msgExchangeFactory.verify();
        componentContext.verify();
        endpointStatus.verify();
    }

    private void cleanup(String[] dirs, String msg) {
        try {
            for (int i = 0; i < dirs.length; i++) {
                cleanUpDir(dirs[i]);
            }
        } catch (Exception e) {
            fail(msg.concat(e.getMessage()));
        }
    }

    private void validateAndPrepareTestFiles(String inputFile, String testFile, String msg) {
        if (!new File(inputFile).exists()) {
            fail("input file:" + inputFile + " does not exist.");
        }
        try {
            if (!new File(testFile).exists()) {
                copyFile(inputFile, testFile);
            }
        } catch (Exception e) {
            fail(msg.concat(e.getMessage()));
        }
    }

    class MyFilter implements FilenameFilter {

        private String mPrefix;

        public MyFilter(String prefix) {
            mPrefix = prefix;
        }

        public boolean accept(File f, String n) {
            if (n != null && n.startsWith(mPrefix)) {
                return true;
            } else {
                return false;
            }
        }
    }
}
