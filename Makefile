D=	t3x9r4
A=	$D.tgz

all:	t0 t.elf t.vm tcvm

t0:	t.c t.t
	cc -static -o t0 t.c

t.vm:	t-vm
	./t-vm <t-vm.t >t.vm

t-vm:	t-vm.t
	./t3 <t-vm.t >t-vm && chmod +x t-vm

tcvm:	tcvm.c
	cc -O2 -g -o tcvm tcvm.c

t.elf:	test
	cp t3 t.elf

test:	t0
	touch t1 t2 t3; chmod +x t1 t2 t3
	./t0 <t.t >t1 && ./t1 <t.t >t2 && ./t2 <t.t >t3 && cmp t2 t3

vmtest:	tcvm
	./tcvm t.vm <t-vm.t >t1.vm
	./tcvm t1.vm <t-vm.t >t2.vm
	./tcvm t2.vm <t-vm.t >t3.vm
	cmp t2.vm t3.vm

mksums: clean
	ls | grep -v $A | grep -v _csums | csum -m >_csums

csums:
	csum -u <_csums >_csums.new && mv -f _csums.new _csums

clean:
	rm -f t0 t1 t2 t3 t1.vm t2.vm t3.vm t-vm a.out tcvm dump *.o *.core \
	$A

arc:	clean
	(cd ..; tar cvf - $D | gzip -9 >$A); mv -f ../$A .
