import re, os, subprocess, pytest

TEST_CORRECT_RESULTS = {
    "addNum": 210,
    "factorial": 120,
    "fibonacci": [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55],
    "lcm": 216,
    "palindrome": "true",
    "prime": [907, 911, 919, 929, 937, 941, 947, 953, 967, 971, 977, 983, 991, 997],
    "rotateArray": [3, 4, 5, 6, 7, 1, 2],
    "unary": 4,
}


def get_test_name(filePath: str):
    baseName = os.path.basename(os.path.normpath(filePath))
    testName, _ = os.path.splitext(baseName)
    return testName

def get_test_result(filePath: str):
    currDir = os.getcwd()
    command = (
        f"python {currDir}\\rukky.py -f {filePath}"
    )
    result = subprocess.run(command, stdout=subprocess.PIPE)
    testOut = result.stdout.decode("utf-8").replace("\r\n", "").replace("\n", "")
    search = re.search("result:\s+(.+)", testOut)
    _, match = search.group().split(":")
    return match.strip()

def verify_path(filePath: str):
    if not os.path.exists(filePath):
        print(f"{filePath}: The file path does not exist")
        assert False
    else:
        return r"" + filePath

def test_sucess():
    # loop through all test files in test folder, verify them and use returned path
    # get test result, get test name
    # use dictionary to get correct result and compare it to test result
    # type cast test result using type of retrieved correct result
    pass


def test_failure():
    pass
