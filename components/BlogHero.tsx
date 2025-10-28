import * as React from "react";
import { SearchInput } from "./SearchInput";

export function BlogHero() {
  return (
    <section className="flex flex-col items-center pt-24 w-full bg-purple-50 max-md:max-w-full">
      <div className="px-8 max-w-full w-[1280px] max-md:px-5">
        <div className="flex flex-col items-center w-full max-md:max-w-full">
          <header className="max-w-full text-center w-[1024px]">
            <div className="flex flex-col w-full max-md:max-w-full">
              <div className="flex items-start self-center text-sm font-medium leading-none text-violet-700 bg-blend-multiply">
                <div className="flex justify-center items-center px-3 py-1 bg-purple-50 rounded-2xl">
                  <span className="self-stretch my-auto text-violet-700">
                    My second brain
                  </span>
                </div>
              </div>
              <h1 className="mt-4 text-5xl font-semibold tracking-tighter leading-none text-indigo-900 max-md:max-w-full max-md:text-4xl">
                Philosophy and ideas
              </h1>
            </div>
            <p className="mt-6 text-xl text-violet-700 max-md:max-w-full">
              My zibaldone of connected ideas and embryonic thoughts
            </p>
          </header>
          <SearchInput />
        </div>
      </div>
    </section>
  );
}
