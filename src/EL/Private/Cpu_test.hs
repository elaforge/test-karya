{-# LANGUAGE OverloadedStrings #-}
module EL.Private.Cpu_test where
import qualified EL.Private.Cpu as Cpu
import EL.Test.Global
import Global


test_cpuinfoPhysical = do
    let f = Cpu.cpuinfoPhysical
    equal (f cpuinfo) 1

cpuinfo :: Text
cpuinfo =
    "processor       : 0\n\
    \vendor_id       : GenuineIntel\n\
    \cpu family      : 6\n\
    \model           : 79\n\
    \model name      : Intel(R) Xeon(R) CPU @ 2.20GHz\n\
    \stepping        : 0\n\
    \microcode       : 0x1\n\
    \cpu MHz         : 2200.000\n\
    \cache size      : 56320 KB\n\
    \physical id     : 0\n\
    \siblings        : 2\n\
    \core id         : 0\n\
    \cpu cores       : 1\n\
    \apicid          : 0\n\
    \initial apicid  : 0\n\
    \fpu             : yes\n\
    \fpu_exception   : yes\n\
    \cpuid level     : 13\n\
    \wp              : yes\n\
    \flags           : fpu vme de pse tsc msr pae mce cx8 apic sep mtrr pge mca cmov pat pse36 clflush mmx fxsr sse sse2 ss ht syscall nx pdpe1gb rdtscp lm constant_tsc rep_good nopl xtopology nonstop_tsc cpuid pni pclmulqdq ssse3 fma cx16 pcid sse4_1 sse4_2 x2apic movbe popcnt aes xsave avx f16c rdrand hypervisor lahf_lm abm 3dnowprefetch invpcid_single pti retpoline fsgsbase tsc_adjust bmi1 hle avx2 smep bmi2 erms invpcid rtm rdseed adx smap xsaveopt\n\
    \bugs            : cpu_meltdown spectre_v1 spectre_v2 spec_store_bypass\n\
    \bogomips        : 4400.00\n\
    \clflush size    : 64\n\
    \cache_alignment : 64\n\
    \address sizes   : 46 bits physical, 48 bits virtual\n\
    \power management:\n\
    \\n\
    \processor       : 1\n\
    \vendor_id       : GenuineIntel\n\
    \cpu family      : 6\n\
    \model           : 79\n\
    \model name      : Intel(R) Xeon(R) CPU @ 2.20GHz\n\
    \stepping        : 0\n\
    \microcode       : 0x1\n\
    \cpu MHz         : 2200.000\n\
    \cache size      : 56320 KB\n\
    \physical id     : 0\n\
    \siblings        : 2\n\
    \core id         : 0\n\
    \cpu cores       : 1\n\
    \apicid          : 1\n\
    \initial apicid  : 1\n\
    \fpu             : yes\n\
    \fpu_exception   : yes\n\
    \cpuid level     : 13\n\
    \wp              : yes\n\
    \flags           : fpu vme de pse tsc msr pae mce cx8 apic sep mtrr pge mca cmov pat pse36 clflush mmx fxsr sse sse2 ss ht syscall nx pdpe1gb rdtscp lm constant_tsc rep_good nopl xtopology nonstop_tsc cpuid pni pclmulqdq ssse3 fma cx16 pcid sse4_1 sse4_2 x2apic movbe popcnt aes xsave avx f16c rdrand hypervisor lahf_lm abm 3dnowprefetch invpcid_single pti retpoline fsgsbase tsc_adjust bmi1 hle avx2 smep bmi2 erms invpcid rtm rdseed adx smap xsaveopt\n\
    \bugs            : cpu_meltdown spectre_v1 spectre_v2 spec_store_bypass\n"
