package it.imolinfo.jbi4corba.test.testprovidercomplex;


/**
* it/imolinfo/jbi4corba/test/testprovidercomplex/_EchoComplexStub.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from provider-complex-jbi4corba-provider/src/main/resources/EchoComplex.idl
* Tuesday, September 11, 2007 3:26:57 PM CEST
*/


//==================================================
public class _EchoComplexStub extends org.omg.CORBA.portable.ObjectImpl implements it.imolinfo.jbi4corba.test.testprovidercomplex.EchoComplex
{

  public String echo (String msg)
  {
            org.omg.CORBA.portable.InputStream $in = null;
            try {
                org.omg.CORBA.portable.OutputStream $out = _request ("echo", true);
                $out.write_string (msg);
                $in = _invoke ($out);
                String $result = $in.read_string ();
                return $result;
            } catch (org.omg.CORBA.portable.ApplicationException $ex) {
                $in = $ex.getInputStream ();
                String _id = $ex.getId ();
                throw new org.omg.CORBA.MARSHAL (_id);
            } catch (org.omg.CORBA.portable.RemarshalException $rm) {
                return echo (msg        );
            } finally {
                _releaseReply ($in);
            }
  } // echo

  public it.imolinfo.jbi4corba.test.testprovidercomplex.EchoVT echoValueType (it.imolinfo.jbi4corba.test.testprovidercomplex.EchoVT e)
  {
            org.omg.CORBA.portable.InputStream $in = null;
            try {
                org.omg.CORBA.portable.OutputStream $out = _request ("echoValueType", true);
                it.imolinfo.jbi4corba.test.testprovidercomplex.EchoVTHelper.write ($out, e);
                $in = _invoke ($out);
                it.imolinfo.jbi4corba.test.testprovidercomplex.EchoVT $result = it.imolinfo.jbi4corba.test.testprovidercomplex.EchoVTHelper.read ($in);
                return $result;
            } catch (org.omg.CORBA.portable.ApplicationException $ex) {
                $in = $ex.getInputStream ();
                String _id = $ex.getId ();
                throw new org.omg.CORBA.MARSHAL (_id);
            } catch (org.omg.CORBA.portable.RemarshalException $rm) {
                return echoValueType (e        );
            } finally {
                _releaseReply ($in);
            }
  } // echoValueType

  public it.imolinfo.jbi4corba.test.testprovidercomplex.MyLong echoValueBoxedTypePrimitive (it.imolinfo.jbi4corba.test.testprovidercomplex.MyLong e)
  {
            org.omg.CORBA.portable.InputStream $in = null;
            try {
                org.omg.CORBA.portable.OutputStream $out = _request ("echoValueBoxedTypePrimitive", true);
                it.imolinfo.jbi4corba.test.testprovidercomplex.MyLongHelper.write ($out, e);
                $in = _invoke ($out);
                it.imolinfo.jbi4corba.test.testprovidercomplex.MyLong $result = it.imolinfo.jbi4corba.test.testprovidercomplex.MyLongHelper.read ($in);
                return $result;
            } catch (org.omg.CORBA.portable.ApplicationException $ex) {
                $in = $ex.getInputStream ();
                String _id = $ex.getId ();
                throw new org.omg.CORBA.MARSHAL (_id);
            } catch (org.omg.CORBA.portable.RemarshalException $rm) {
                return echoValueBoxedTypePrimitive (e        );
            } finally {
                _releaseReply ($in);
            }
  } // echoValueBoxedTypePrimitive

  public it.imolinfo.jbi4corba.test.testprovidercomplex.MySequence echoValueBoxedTypeComplex (it.imolinfo.jbi4corba.test.testprovidercomplex.MySequence e)
  {
            org.omg.CORBA.portable.InputStream $in = null;
            try {
                org.omg.CORBA.portable.OutputStream $out = _request ("echoValueBoxedTypeComplex", true);
                it.imolinfo.jbi4corba.test.testprovidercomplex.MySequenceHelper.write ($out, e);
                $in = _invoke ($out);
                it.imolinfo.jbi4corba.test.testprovidercomplex.MySequence $result = it.imolinfo.jbi4corba.test.testprovidercomplex.MySequenceHelper.read ($in);
                return $result;
            } catch (org.omg.CORBA.portable.ApplicationException $ex) {
                $in = $ex.getInputStream ();
                String _id = $ex.getId ();
                throw new org.omg.CORBA.MARSHAL (_id);
            } catch (org.omg.CORBA.portable.RemarshalException $rm) {
                return echoValueBoxedTypeComplex (e        );
            } finally {
                _releaseReply ($in);
            }
  } // echoValueBoxedTypeComplex

  public it.imolinfo.jbi4corba.test.testprovidercomplex.EchoStruct echoStruct (it.imolinfo.jbi4corba.test.testprovidercomplex.EchoStruct es)
  {
            org.omg.CORBA.portable.InputStream $in = null;
            try {
                org.omg.CORBA.portable.OutputStream $out = _request ("echoStruct", true);
                it.imolinfo.jbi4corba.test.testprovidercomplex.EchoStructHelper.write ($out, es);
                $in = _invoke ($out);
                it.imolinfo.jbi4corba.test.testprovidercomplex.EchoStruct $result = it.imolinfo.jbi4corba.test.testprovidercomplex.EchoStructHelper.read ($in);
                return $result;
            } catch (org.omg.CORBA.portable.ApplicationException $ex) {
                $in = $ex.getInputStream ();
                String _id = $ex.getId ();
                throw new org.omg.CORBA.MARSHAL (_id);
            } catch (org.omg.CORBA.portable.RemarshalException $rm) {
                return echoStruct (es        );
            } finally {
                _releaseReply ($in);
            }
  } // echoStruct

  public int[] echoSequence (int[] es)
  {
            org.omg.CORBA.portable.InputStream $in = null;
            try {
                org.omg.CORBA.portable.OutputStream $out = _request ("echoSequence", true);
                it.imolinfo.jbi4corba.test.testprovidercomplex.SeqLongHelper.write ($out, es);
                $in = _invoke ($out);
                int $result[] = it.imolinfo.jbi4corba.test.testprovidercomplex.SeqLongHelper.read ($in);
                return $result;
            } catch (org.omg.CORBA.portable.ApplicationException $ex) {
                $in = $ex.getInputStream ();
                String _id = $ex.getId ();
                throw new org.omg.CORBA.MARSHAL (_id);
            } catch (org.omg.CORBA.portable.RemarshalException $rm) {
                return echoSequence (es        );
            } finally {
                _releaseReply ($in);
            }
  } // echoSequence

  public it.imolinfo.jbi4corba.test.testprovidercomplex.MySequence echoSequenceValueType (it.imolinfo.jbi4corba.test.testprovidercomplex.MySequence es)
  {
            org.omg.CORBA.portable.InputStream $in = null;
            try {
                org.omg.CORBA.portable.OutputStream $out = _request ("echoSequenceValueType", true);
                it.imolinfo.jbi4corba.test.testprovidercomplex.MySequenceHelper.write ($out, es);
                $in = _invoke ($out);
                it.imolinfo.jbi4corba.test.testprovidercomplex.MySequence $result = it.imolinfo.jbi4corba.test.testprovidercomplex.MySequenceHelper.read ($in);
                return $result;
            } catch (org.omg.CORBA.portable.ApplicationException $ex) {
                $in = $ex.getInputStream ();
                String _id = $ex.getId ();
                throw new org.omg.CORBA.MARSHAL (_id);
            } catch (org.omg.CORBA.portable.RemarshalException $rm) {
                return echoSequenceValueType (es        );
            } finally {
                _releaseReply ($in);
            }
  } // echoSequenceValueType

  public it.imolinfo.jbi4corba.test.testprovidercomplex.EchoStruct[] echoSequenceSeqEchoStruct (it.imolinfo.jbi4corba.test.testprovidercomplex.EchoStruct[] es)
  {
            org.omg.CORBA.portable.InputStream $in = null;
            try {
                org.omg.CORBA.portable.OutputStream $out = _request ("echoSequenceSeqEchoStruct", true);
                it.imolinfo.jbi4corba.test.testprovidercomplex.SeqEchoStructHelper.write ($out, es);
                $in = _invoke ($out);
                it.imolinfo.jbi4corba.test.testprovidercomplex.EchoStruct $result[] = it.imolinfo.jbi4corba.test.testprovidercomplex.SeqEchoStructHelper.read ($in);
                return $result;
            } catch (org.omg.CORBA.portable.ApplicationException $ex) {
                $in = $ex.getInputStream ();
                String _id = $ex.getId ();
                throw new org.omg.CORBA.MARSHAL (_id);
            } catch (org.omg.CORBA.portable.RemarshalException $rm) {
                return echoSequenceSeqEchoStruct (es        );
            } finally {
                _releaseReply ($in);
            }
  } // echoSequenceSeqEchoStruct

  public it.imolinfo.jbi4corba.test.testprovidercomplex.MySequence[] echoSequenceSeqMySequence (it.imolinfo.jbi4corba.test.testprovidercomplex.MySequence[] es)
  {
            org.omg.CORBA.portable.InputStream $in = null;
            try {
                org.omg.CORBA.portable.OutputStream $out = _request ("echoSequenceSeqMySequence", true);
                it.imolinfo.jbi4corba.test.testprovidercomplex.SeqMySequenceHelper.write ($out, es);
                $in = _invoke ($out);
                it.imolinfo.jbi4corba.test.testprovidercomplex.MySequence $result[] = it.imolinfo.jbi4corba.test.testprovidercomplex.SeqMySequenceHelper.read ($in);
                return $result;
            } catch (org.omg.CORBA.portable.ApplicationException $ex) {
                $in = $ex.getInputStream ();
                String _id = $ex.getId ();
                throw new org.omg.CORBA.MARSHAL (_id);
            } catch (org.omg.CORBA.portable.RemarshalException $rm) {
                return echoSequenceSeqMySequence (es        );
            } finally {
                _releaseReply ($in);
            }
  } // echoSequenceSeqMySequence

  public it.imolinfo.jbi4corba.test.testprovidercomplex.EchoStruct echoExceptionThrown (it.imolinfo.jbi4corba.test.testprovidercomplex.EchoStruct es) throws it.imolinfo.jbi4corba.test.testprovidercomplex.EchoComplexException
  {
            org.omg.CORBA.portable.InputStream $in = null;
            try {
                org.omg.CORBA.portable.OutputStream $out = _request ("echoExceptionThrown", true);
                it.imolinfo.jbi4corba.test.testprovidercomplex.EchoStructHelper.write ($out, es);
                $in = _invoke ($out);
                it.imolinfo.jbi4corba.test.testprovidercomplex.EchoStruct $result = it.imolinfo.jbi4corba.test.testprovidercomplex.EchoStructHelper.read ($in);
                return $result;
            } catch (org.omg.CORBA.portable.ApplicationException $ex) {
                $in = $ex.getInputStream ();
                String _id = $ex.getId ();
                if (_id.equals ("IDL:it/imolinfo/jbi4corba/test/testprovidercomplex/EchoComplexException:1.0"))
                    throw it.imolinfo.jbi4corba.test.testprovidercomplex.EchoComplexExceptionHelper.read ($in);
                else
                    throw new org.omg.CORBA.MARSHAL (_id);
            } catch (org.omg.CORBA.portable.RemarshalException $rm) {
                return echoExceptionThrown (es        );
            } finally {
                _releaseReply ($in);
            }
  } // echoExceptionThrown

  public it.imolinfo.jbi4corba.test.testprovidercomplex.EchoStruct echoExceptionNotThrown (it.imolinfo.jbi4corba.test.testprovidercomplex.EchoStruct es) throws it.imolinfo.jbi4corba.test.testprovidercomplex.EchoComplexException
  {
            org.omg.CORBA.portable.InputStream $in = null;
            try {
                org.omg.CORBA.portable.OutputStream $out = _request ("echoExceptionNotThrown", true);
                it.imolinfo.jbi4corba.test.testprovidercomplex.EchoStructHelper.write ($out, es);
                $in = _invoke ($out);
                it.imolinfo.jbi4corba.test.testprovidercomplex.EchoStruct $result = it.imolinfo.jbi4corba.test.testprovidercomplex.EchoStructHelper.read ($in);
                return $result;
            } catch (org.omg.CORBA.portable.ApplicationException $ex) {
                $in = $ex.getInputStream ();
                String _id = $ex.getId ();
                if (_id.equals ("IDL:it/imolinfo/jbi4corba/test/testprovidercomplex/EchoComplexException:1.0"))
                    throw it.imolinfo.jbi4corba.test.testprovidercomplex.EchoComplexExceptionHelper.read ($in);
                else
                    throw new org.omg.CORBA.MARSHAL (_id);
            } catch (org.omg.CORBA.portable.RemarshalException $rm) {
                return echoExceptionNotThrown (es        );
            } finally {
                _releaseReply ($in);
            }
  } // echoExceptionNotThrown


  // UNSUPPORTED : string echoAbstractValueType(in AFoo n);
  public it.imolinfo.jbi4corba.test.testprovidercomplex.StructOfStruct echoStructOfStruct (it.imolinfo.jbi4corba.test.testprovidercomplex.StructOfStruct v)
  {
            org.omg.CORBA.portable.InputStream $in = null;
            try {
                org.omg.CORBA.portable.OutputStream $out = _request ("echoStructOfStruct", true);
                it.imolinfo.jbi4corba.test.testprovidercomplex.StructOfStructHelper.write ($out, v);
                $in = _invoke ($out);
                it.imolinfo.jbi4corba.test.testprovidercomplex.StructOfStruct $result = it.imolinfo.jbi4corba.test.testprovidercomplex.StructOfStructHelper.read ($in);
                return $result;
            } catch (org.omg.CORBA.portable.ApplicationException $ex) {
                $in = $ex.getInputStream ();
                String _id = $ex.getId ();
                throw new org.omg.CORBA.MARSHAL (_id);
            } catch (org.omg.CORBA.portable.RemarshalException $rm) {
                return echoStructOfStruct (v        );
            } finally {
                _releaseReply ($in);
            }
  } // echoStructOfStruct

  public it.imolinfo.jbi4corba.test.testprovidercomplex.VTPrimi echoVTPrimi (it.imolinfo.jbi4corba.test.testprovidercomplex.VTPrimi v)
  {
            org.omg.CORBA.portable.InputStream $in = null;
            try {
                org.omg.CORBA.portable.OutputStream $out = _request ("echoVTPrimi", true);
                it.imolinfo.jbi4corba.test.testprovidercomplex.VTPrimiHelper.write ($out, v);
                $in = _invoke ($out);
                it.imolinfo.jbi4corba.test.testprovidercomplex.VTPrimi $result = it.imolinfo.jbi4corba.test.testprovidercomplex.VTPrimiHelper.read ($in);
                return $result;
            } catch (org.omg.CORBA.portable.ApplicationException $ex) {
                $in = $ex.getInputStream ();
                String _id = $ex.getId ();
                throw new org.omg.CORBA.MARSHAL (_id);
            } catch (org.omg.CORBA.portable.RemarshalException $rm) {
                return echoVTPrimi (v        );
            } finally {
                _releaseReply ($in);
            }
  } // echoVTPrimi

  public it.imolinfo.jbi4corba.test.testprovidercomplex.VTPrimiSeq echoVTPrimiSeq (it.imolinfo.jbi4corba.test.testprovidercomplex.VTPrimiSeq v)
  {
            org.omg.CORBA.portable.InputStream $in = null;
            try {
                org.omg.CORBA.portable.OutputStream $out = _request ("echoVTPrimiSeq", true);
                it.imolinfo.jbi4corba.test.testprovidercomplex.VTPrimiSeqHelper.write ($out, v);
                $in = _invoke ($out);
                it.imolinfo.jbi4corba.test.testprovidercomplex.VTPrimiSeq $result = it.imolinfo.jbi4corba.test.testprovidercomplex.VTPrimiSeqHelper.read ($in);
                return $result;
            } catch (org.omg.CORBA.portable.ApplicationException $ex) {
                $in = $ex.getInputStream ();
                String _id = $ex.getId ();
                throw new org.omg.CORBA.MARSHAL (_id);
            } catch (org.omg.CORBA.portable.RemarshalException $rm) {
                return echoVTPrimiSeq (v        );
            } finally {
                _releaseReply ($in);
            }
  } // echoVTPrimiSeq

  public it.imolinfo.jbi4corba.test.testprovidercomplex.ValueTypeOfValueType echoValueTypeOfValueType (it.imolinfo.jbi4corba.test.testprovidercomplex.ValueTypeOfValueType v)
  {
            org.omg.CORBA.portable.InputStream $in = null;
            try {
                org.omg.CORBA.portable.OutputStream $out = _request ("echoValueTypeOfValueType", true);
                it.imolinfo.jbi4corba.test.testprovidercomplex.ValueTypeOfValueTypeHelper.write ($out, v);
                $in = _invoke ($out);
                it.imolinfo.jbi4corba.test.testprovidercomplex.ValueTypeOfValueType $result = it.imolinfo.jbi4corba.test.testprovidercomplex.ValueTypeOfValueTypeHelper.read ($in);
                return $result;
            } catch (org.omg.CORBA.portable.ApplicationException $ex) {
                $in = $ex.getInputStream ();
                String _id = $ex.getId ();
                throw new org.omg.CORBA.MARSHAL (_id);
            } catch (org.omg.CORBA.portable.RemarshalException $rm) {
                return echoValueTypeOfValueType (v        );
            } finally {
                _releaseReply ($in);
            }
  } // echoValueTypeOfValueType

  public it.imolinfo.jbi4corba.test.testprovidercomplex.ValueTypeOfStruct echoValueTypeOfStruct (it.imolinfo.jbi4corba.test.testprovidercomplex.ValueTypeOfStruct v)
  {
            org.omg.CORBA.portable.InputStream $in = null;
            try {
                org.omg.CORBA.portable.OutputStream $out = _request ("echoValueTypeOfStruct", true);
                it.imolinfo.jbi4corba.test.testprovidercomplex.ValueTypeOfStructHelper.write ($out, v);
                $in = _invoke ($out);
                it.imolinfo.jbi4corba.test.testprovidercomplex.ValueTypeOfStruct $result = it.imolinfo.jbi4corba.test.testprovidercomplex.ValueTypeOfStructHelper.read ($in);
                return $result;
            } catch (org.omg.CORBA.portable.ApplicationException $ex) {
                $in = $ex.getInputStream ();
                String _id = $ex.getId ();
                throw new org.omg.CORBA.MARSHAL (_id);
            } catch (org.omg.CORBA.portable.RemarshalException $rm) {
                return echoValueTypeOfStruct (v        );
            } finally {
                _releaseReply ($in);
            }
  } // echoValueTypeOfStruct

  public it.imolinfo.jbi4corba.test.testprovidercomplex.StructOfValuetype echoStructOfValuetype (it.imolinfo.jbi4corba.test.testprovidercomplex.StructOfValuetype v)
  {
            org.omg.CORBA.portable.InputStream $in = null;
            try {
                org.omg.CORBA.portable.OutputStream $out = _request ("echoStructOfValuetype", true);
                it.imolinfo.jbi4corba.test.testprovidercomplex.StructOfValuetypeHelper.write ($out, v);
                $in = _invoke ($out);
                it.imolinfo.jbi4corba.test.testprovidercomplex.StructOfValuetype $result = it.imolinfo.jbi4corba.test.testprovidercomplex.StructOfValuetypeHelper.read ($in);
                return $result;
            } catch (org.omg.CORBA.portable.ApplicationException $ex) {
                $in = $ex.getInputStream ();
                String _id = $ex.getId ();
                throw new org.omg.CORBA.MARSHAL (_id);
            } catch (org.omg.CORBA.portable.RemarshalException $rm) {
                return echoStructOfValuetype (v        );
            } finally {
                _releaseReply ($in);
            }
  } // echoStructOfValuetype

  // Type-specific CORBA::Object operations
  private static String[] __ids = {
    "IDL:it/imolinfo/jbi4corba/test/testprovidercomplex/EchoComplex:1.0"};

  public String[] _ids ()
  {
    return (String[])__ids.clone ();
  }

  private void readObject (java.io.ObjectInputStream s) throws java.io.IOException
  {
     String str = s.readUTF ();
     String[] args = null;
     java.util.Properties props = null;
     org.omg.CORBA.Object obj = org.omg.CORBA.ORB.init (args, props).string_to_object (str);
     org.omg.CORBA.portable.Delegate delegate = ((org.omg.CORBA.portable.ObjectImpl) obj)._get_delegate ();
     _set_delegate (delegate);
  }

  private void writeObject (java.io.ObjectOutputStream s) throws java.io.IOException
  {
     String[] args = null;
     java.util.Properties props = null;
     String str = org.omg.CORBA.ORB.init (args, props).object_to_string (this);
     s.writeUTF (str);
  }
} // class _EchoComplexStub